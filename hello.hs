{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings #-}
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

myCss :: ByteString = "\
  \.active {color: #00ff00;}\
  \"

main :: IO ()
main = mainWidgetWithCss myCss stuff

-- flipflop a b e = do
--   tog <- toggle False e
--   mapDyn (bool a b) tog

flipflop :: (_) => a -> a -> Event t b -> m (Dynamic t a)
flipflop a b e = toggle False e >>= mapDyn (bool a b)

mybutton :: MonadWidget t m => m ()
mybutton = do
  rec (bel, _) <- el' "button" $ dynText btext
      let clicked = domEvent Click bel
      btext <- flipflop "unclicked" "Clicked!" clicked
  return ()

counterButton :: MonadWidget t m => m (Event t Int)
counterButton = do
  (bel, _) <- el' "button" $ text "New"
  let clicks = domEvent Click bel
  cnt <- count clicks
  return $ updated cnt

linkButton :: MonadWidget t m => Dynamic t k -> Dynamic t (Map k (Dynamic t (Int, Bool))) -> m (Event t (k, (Map k (Dynamic t (Int, Bool)))))
linkButton to newPort = do
  (bel, _) <- el' "button" $ text "Link"
  let clicks = domEvent Click bel
  return $ attachDyn to $ tagDyn newPort clicks

mkNode :: (MonadWidget t m, _) => k -> Dynamic t (Map k (Dynamic t (Int, Bool))) -> m (Dynamic t (Int, Bool))
mkNode k p = do
  let ports = joinDynThroughMap p
  levelIn <- forDyn ports $ (foldl' max 0) . fmap fst 
  levelOut <- mapDyn succ levelIn
  levelName <- mapDyn show levelIn
  -- (portNames :: Dynamic t String) <- forDyn ports $ (intercalate ",") . elems . (fmap (show . fst))
  (portNames :: Dynamic t String) <- forDyn ports $ (intercalate ",") . (fmap show) . Map.keys
  let constLabel = constDyn $ "I am #" <> show k <> " @"
  label <- mconcatDyn [constLabel, levelName, constDyn "\n", portNames]
  atRoot <- mapDyn (== 0) levelIn
  rootAttr <- forDyn atRoot (bool ("disabled" =: "true") Map.empty)
  stateIn <- mapDyn (and . fmap snd) ports
  rec stateOut <- chooseDyn stateIn clickToggle atRoot
      stateAttr <- forDyn stateOut (bool Map.empty ("class" =: "active"))
      nodeAttrs <- mconcatDyn [stateAttr, rootAttr]
      (bel, _) <- elDynAttr' "button" nodeAttrs $ do
        dynText label
      clickToggle <- toggle False $ domEvent Click bel
  combineDyn (,) levelOut stateOut

bunchaButtons :: MonadWidget t m => m ()
bunchaButtons = do
  rec (cnt, newp) <- divClass "control" $ do
        c <- counterButton
        (Dropdown from _) <- dropdown 0 keyNames def
        (Dropdown to _)  <- dropdown 0 keyNames def
        newPort <- combineDyn getPort from nodeOut
        p <- linkButton to newPort
        return (c, p)
      let bmanip = leftmost [ fmap newKey cnt, fmap addPort newp ]
      (nodeIn :: Dynamic t (Map Int (Map Int (Dynamic t (Int, Bool))))) <- foldDyn ($) Map.empty bmanip
      (nodeOut :: Dynamic t (Map Int (Dynamic t (Int, Bool)))) <- listWithKey nodeIn mkNode
      keyNames <- forDyn nodeOut $ (fmap show) . (mapWithKey const)
  return ()
  where
    newKey k = Map.insert k Map.empty
    getPort from m = maybe Map.empty (from =:) (Map.lookup from m)
    addPort (to, p) = Map.adjust (p <>) to

stuff :: MonadWidget t m => m ()
stuff = do
  text "Hello, world!"
  text "blah"
  bunchaButtons
  -- mybutton
