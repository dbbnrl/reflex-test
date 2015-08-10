{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings #-}
import Data.Bool
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, mapWithKey)
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

linkButton :: MonadWidget t m => Dynamic t k -> Dynamic t k -> m (Event t (k, k))
linkButton from to = do
  (bel, _) <- el' "button" $ text "Link"
  let clicks = domEvent Click bel
  return $ attachDyn from $ tagDyn to clicks

mkNode :: (MonadWidget t m, _) => k -> Dynamic t [(Int, Bool)] -> m (Dynamic t (Int, Bool))
mkNode k ports = do
  levelIn <- forDyn ports $ (foldl' max 0) . fmap fst 
  levelOut <- mapDyn succ levelIn
  levelName <- mapDyn show levelIn
  (portNames :: Dynamic t String) <- forDyn ports $ (intercalate ",") . (fmap (show . fst))
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
  rec (cnt, lnk) <- divClass "control" $ do
        c <- counterButton
        (Dropdown f _) <- dropdown 0 keyNames def
        (Dropdown t _)  <- dropdown 0 keyNames def
        l <- linkButton f t
        return (c, l)
      let bmanip = leftmost [ fmap newKey cnt ]
      nodeIn <- foldDyn ($) Map.empty bmanip
      keyNames <- forDyn nodeIn $ (fmap show) . (mapWithKey const)
  nodeOut <- listWithKey nodeIn mkNode
  return ()
  where
    newKey k = Map.insert k []

stuff :: MonadWidget t m => m ()
stuff = do
  text "Hello, world!"
  text "blah"
  bunchaButtons
  -- mybutton
