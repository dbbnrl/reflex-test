{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings, BangPatterns, RankNTypes #-}
import Prelude hiding(lookup)
import Data.Bool
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, elems, keys, lookup, insert, adjust, delete, mapWithKey, mapMaybeWithKey)
import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.List (intercalate)

chooseDyn :: (_) => Dynamic t a -> Dynamic t a -> Dynamic t Bool -> m (Dynamic t a)
chooseDyn x y b = joinDyn <$> mapDyn (bool x y) b

-- Node contains the state of a node in the network.  At the moment it consists of
-- the node's "level" in the DAG (longest chain from a node with no inputs to this node),
-- and the current boolean output of this node.
data Node = Node {nLevel :: !Int, nVal :: !Bool}

-- A Port is a group of incoming edges to a node.  Each edge carries the current state
-- of some other node.  Our value is the logical AND of our Port's values.
-- Eventually the goal is to have multiple Ports, which are OR'ed together.
type Port t = Map Int (Dynamic t Node)

counterButton :: MonadWidget t m => m (Event t Int)
counterButton = do
  clicks <- button "Create Node"
  cnt <- count clicks
  return $ updated cnt

delButton :: MonadWidget t m => Dynamic t Int -> m (Event t Int)
delButton which = do
  clicks <- button "Delete Node"
  return $ tagDyn which clicks

pairButton :: MonadWidget t m => String -> Dynamic t Int -> Dynamic t Int -> m (Event t (Int, Int))
pairButton label to from = do
  clicks <- button label
  return $ attachDyn to $ tagDyn from clicks

feedbackText :: MonadWidget t m => Dynamic t (Either String String) -> m ()
feedbackText fb = do
  msg <- forDyn fb $ either id id  -- Slightly silly
  msgAttr <- forDyn fb $ (("id" =: "feedback") <>) . either (const ("class" =: "error")) (const Map.empty)
  elDynAttr "span" msgAttr $ dynText msg

-- Return 4 different event streams for (new node, delete node, new link, delete link)
controlPanel :: MonadWidget t m => (Dynamic t (Either String String), Dynamic t (Map Int String)) ->
                                   m (Event t Int, Event t Int, Event t (Int, Int), Event t (Int, Int))
controlPanel ~(userFeedback, keyNames) = do
  divClass "control" $ do
    (c, d) <- divClass "node-control" $ do
      c <- counterButton
      (Dropdown which _) <- dropdown 0 keyNames def
      d <- delButton which
      return (c, d)
    (l, u) <- divClass "link-control" $ do
      text "From:"
      (Dropdown from _) <- dropdown 0 keyNames def
      text "To:"
      (Dropdown to _)  <- dropdown 0 keyNames def
      l <- pairButton "Link" to from
      u <- pairButton "Unlink" to from
      return (l, u)
    feedbackText userFeedback
    return (c, d, l, u)

-- Node widget.  Compute the node's state (and rendering) from its incoming edge values.
mkNode :: (MonadWidget t m) => Int -> Dynamic t (Port t) -> m (Dynamic t Node)
mkNode k s = do
  let sources = joinDynThroughMap s   -- Collapse nested Dynamics to simplify
  levelIn <- forDyn sources $ (foldl' max 0) . fmap nLevel 
  levelOut <- mapDyn succ levelIn
  offset <- mapDyn (*100) levelOut
  levelName <- mapDyn show levelIn
  sourceLabels <- forDyn sources $ (intercalate ",") . (fmap show) . keys
  let constLabel = constDyn $ "I am #" <> show k <> " @Level "
  label1 <- mconcatDyn [constLabel, levelName]
  label2 <- mconcatDyn [constDyn "Sources: ", sourceLabels]
  atRoot <- mapDyn (== 0) levelIn
  rootAttr <- forDyn atRoot (bool ("disabled" =: "true") Map.empty)
  topAttr <- forDyn offset (\x -> ("style" =: ("top:" <> show x <> "px")))
  stateIn <- mapDyn (and . fmap nVal) sources  -- Actually compute our boolean state from inputs
  rec stateOut <- chooseDyn stateIn clickToggle atRoot
      stateAttr <- forDyn stateOut (bool Map.empty ("class" =: "active"))
      nodeAttrs <- mconcatDyn [stateAttr, rootAttr, topAttr]
      (bel, _) <- elDynAttr' "button" nodeAttrs $ do
        el "p" $ dynText label1
        el "p" $ dynText label2
      clickToggle <- toggle False $ domEvent Click bel
  combineDyn Node levelOut stateOut

-- mkNetwork creates the DAG and updates it in response to received events.
-- The flow of data *within* the DAG is automatically handled by the fact that DAG edges
-- are Dynamics.
mkNetwork :: MonadWidget t m => (Event t Int, Event t Int, Event t (Int, Int), Event t (Int, Int)) ->
                                m (Dynamic t (Either String String), Dynamic t (Map Int String))
mkNetwork events = do
  let (newClicked, delClicked, linkClicked, unlinkClicked) = events
  rec -- leftmost should be fine since user can't click simultaneous buttons, right?
      let chooseEvent = leftmost [ fmap newNode newClicked,
                                   fmap delNode delClicked,
                                   pushAlways (addToPort nodeState) linkClicked,
                                   fmap delFromPort unlinkClicked ]
          -- manipEvent carries network-manipulating functions, derived from input events
          manipEvent = fmap fst chooseEvent
          -- feedbackEvent carries feedback messages for the user (success or failure)
          feedbackEvent = fmap snd chooseEvent
      -- network describes the network of links.  It is a Map from destination node IDs to Ports.
      (network :: Dynamic t (Map Int (Port t))) <- foldDyn ($) Map.empty manipEvent
      -- nodeState is the current state of each node.  It Maps from node ID to Dynamic Node.
      (nodeState :: Dynamic t (Map Int (Dynamic t Node))) <- divClass "nodes" $ listWithKey network mkNode
  keyNames <- forDyn nodeState $ (fmap show) . (mapWithKey const)
  userFeedback <- holdDyn (Right "") feedbackEvent
  return (userFeedback, keyNames)  -- Control panel needs these to render
  where
    newNode k = (insert k Map.empty,
                 Right ("Created node " <> show k))
    -- delNode is a bit ugly b/c we need to manually clean up all outgoing edges from the deleted node
    delNode k = (mapMaybeWithKey (\k' p -> if (k == k') then Nothing else Just (delete k p)),
                 Right "")
    -- addToPort needs the current nodeState to perform validation (can't create loops).
    -- It is monadic because it needs to sample that current state.
    addToPort nodeState (to, from) = do
      cs <- sample $ current nodeState  -- current state (contains Dynamics)
      js <- sample $ current $ joinDynThroughMap nodeState  -- current joined state (flat)
      let levels = nLevel <$> js
          ~(Just n) = lookup from cs  -- Only used if lookup would succeed
          manip = adjust ((from =: n) <>) to
          valid = (>=) <$> (lookup to levels) <*> (lookup from levels)  -- valid if to >= from
      case valid of
        Just True  -> return (manip, Right "")
        Just False -> return (   id,  Left "No loops allowed!")
        Nothing    -> return (   id, Right "")
    delFromPort (to, from) = (adjust (delete from) to,
                              Right "")

-- Put it all together
graphApp :: MonadWidget t m => m ()
graphApp = do
  el "h1" $ text "Dynamic Graph Test"
  rec events <- controlPanel feedback
      feedback <- mkNetwork events
  return ()

myCss :: ByteString = "\
  \.nodes button {position: relative; float: left}\
  \.active {color: #00ff00;}\
  \#feedback {color: green;}\
  \#feedback.error {color: red;}\
  \"

main :: IO ()
main = mainWidgetWithCss myCss graphApp
