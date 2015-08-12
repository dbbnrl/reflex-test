{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings, BangPatterns #-}
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

data Node = Node {nLevel :: !Int, nVal :: !Bool}

counterButton :: MonadWidget t m => m (Event t Int)
counterButton = do
  (bel, _) <- el' "button" $ text "New"
  let clicks = domEvent Click bel
  cnt <- count clicks
  return $ updated cnt

linkButton :: MonadWidget t m => Dynamic t k -> Dynamic t (Map k (Dynamic t Node)) -> m (Event t (k, (Map k (Dynamic t Node))))
linkButton to newPort = do
  (bel, _) <- el' "button" $ text "Link"
  let clicks = domEvent Click bel
  return $ attachDyn to $ tagDyn newPort clicks

controlPanel :: (MonadWidget t m, _) => Dynamic t (Map k (Dynamic t Node)) -> m (Event t Int, Event t (k, (Map k (Dynamic t Node))))
controlPanel state = do
  keyNames <- forDyn state $ (fmap show) . (mapWithKey const)
  divClass "control" $ do
    c <- counterButton
    (Dropdown from _) <- dropdown 0 keyNames def
    (Dropdown to _)  <- dropdown 0 keyNames def
    newPort <- combineDyn getPort from state
    p <- linkButton to newPort
    return (c, p)
  where
    getPort from m = maybe Map.empty (from =:) (Map.lookup from m)

mkNode :: (MonadWidget t m, _) => k -> Dynamic t (Map k (Dynamic t Node)) -> m (Dynamic t Node)
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
  rec (newClicked, linkClicked) <- controlPanel stateOut
      let stateManip = leftmost [ fmap newNode newClicked, fmap addPort linkClicked ]
      (stateIn :: Dynamic t (Map Int (Map Int (Dynamic t Node)))) <- foldDyn ($) Map.empty stateManip
      (stateOut :: Dynamic t (Map Int (Dynamic t Node))) <- listWithKey stateIn mkNode
  return ()
  where
    newNode k = Map.insert k Map.empty
    addPort (to, p) = Map.adjust (p <>) to

body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Dynamic Graph Test"
  network

myCss :: ByteString = "\
  \.active {color: #00ff00;}\
  \"

main :: IO ()
main = mainWidgetWithCss myCss body
