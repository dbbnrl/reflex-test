{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings, BangPatterns, RankNTypes #-}
import Data.Bool
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, mapWithKey, elems)
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
  (bel, _) <- el' "button" $ text "Create Node"
  let clicks = domEvent Click bel
  cnt <- count clicks
  return $ updated cnt

delButton :: MonadWidget t m => Dynamic t Int -> m (Event t Int)
delButton which = do
  (bel, _) <- el' "button" $ text "Delete Node:"
  let clicks = domEvent Click bel
  return $ tagDyn which clicks

linkButton :: MonadWidget t m => Dynamic t Int -> Dynamic t (Port t) -> m (Event t (Int, (Port t)))
linkButton to newPort = do
  (bel, _) <- el' "button" $ text "Link"
  let clicks = domEvent Click bel
  return $ attachDyn to $ tagDyn newPort clicks

unlinkButton :: MonadWidget t m => Dynamic t Int -> Dynamic t Int -> m (Event t (Int, Int))
unlinkButton to from = do
  (bel, _) <- el' "button" $ text "Unlink"
  let clicks = domEvent Click bel
  return $ attachDyn to $ tagDyn from clicks

controlPanel :: (MonadWidget t m) => Dynamic t (Map Int (Dynamic t Node))-> m (Event t Int, Event t Int, Event t (Int, (Port t)), Event t (Int, Int))
controlPanel state = do
  keyNames <- forDyn state $ (fmap show) . (mapWithKey const)
  divClass "control" $ do
    (c, d) <- divClass "node-control" $ do
      c <- counterButton
      d <- delButton which
      (Dropdown which _) <- dropdown 0 keyNames def
      return (c, d)
    (l, u) <- divClass "link-control" $ do
      text "From:"
      (Dropdown from _) <- dropdown 0 keyNames def
      text "To:"
      (Dropdown to _)  <- dropdown 0 keyNames def
      newPort <- combineDyn getPort from state
      l <- linkButton to newPort
      u <- unlinkButton to from
      return (l, u)
    return (c, d, l, u)
  where
    getPort from m = maybe Map.empty (from =:) (Map.lookup from m)

mkNode :: (MonadWidget t m) => Int -> Dynamic t (Port t) -> m (Dynamic t Node)
mkNode k s = do
  let sources = joinDynThroughMap s
  levelIn <- forDyn sources $ (foldl' max 0) . fmap nLevel 
  levelOut <- mapDyn succ levelIn
  levelName <- mapDyn show levelIn
  (sourceLabels :: Dynamic t String) <- forDyn sources $ (intercalate ",") . (fmap show) . Map.keys
  let constLabel = constDyn $ "I am #" <> show k <> " @Level "
  label1 <- mconcatDyn [constLabel, levelName]
  label2 <- mconcatDyn [constDyn "Sources: ", sourceLabels]
  atRoot <- mapDyn (== 0) levelIn
  rootAttr <- forDyn atRoot (bool ("disabled" =: "true") Map.empty)
  stateIn <- mapDyn (and . fmap nVal) sources
  rec stateOut <- chooseDyn stateIn clickToggle atRoot
      stateAttr <- forDyn stateOut (bool Map.empty ("class" =: "active"))
      nodeAttrs <- mconcatDyn [stateAttr, rootAttr]
      (bel, _) <- elDynAttr' "button" nodeAttrs $ do
        el "p" $ dynText label1
        el "p" $ dynText label2
      clickToggle <- toggle False $ domEvent Click bel
  combineDyn Node levelOut stateOut

network :: MonadWidget t m => m ()
network = do
  rec (newClicked, delClicked, linkClicked, unlinkClicked) <- controlPanel stateOut
      -- leftmost should be fine since user can't click simultaneous buttons, right?
      let stateManip = leftmost [ fmap newNode newClicked,
                                  fmap delNode delClicked,
                                  fmap addToPort linkClicked,
                                  fmap delFromPort unlinkClicked ]
      -- stateIn describes the network of links.  It is a Map from destination node IDs to Ports.
      (stateIn :: Dynamic t (Map Int (Port t))) <- foldDyn ($) Map.empty stateManip
      -- stateOut is the current state of each node.  It Maps from node ID to Node.
      (stateOut :: Dynamic t (Map Int (Dynamic t Node))) <- listWithKey stateIn mkNode
  return ()
  where
    newNode k = Map.insert k Map.empty
    -- delNode is a bit ugly b/c we need to manually clean up all outgoing edges from the deleted node
    delNode k = Map.mapMaybeWithKey (\k' p -> if (k == k') then Nothing else Just (Map.delete k p))
    addToPort (to, p) = Map.adjust (p <>) to
    delFromPort (to, from) = Map.adjust (Map.delete from) to

body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Dynamic Graph Test"
  network

myCss :: ByteString = "\
  \.active {color: #00ff00;}\
  \"

main :: IO ()
main = mainWidgetWithCss myCss body
