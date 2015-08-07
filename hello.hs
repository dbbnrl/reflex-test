{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures, OverloadedStrings #-}
import Data.Bool
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Foldable (foldl')
import Data.Monoid ((<>))
import Data.ByteString (ByteString)

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
  rec (bel, _) <- el' "button" $ display cnt
      let clicks = domEvent Click bel
      cnt <- count clicks
  return $ updated cnt

mkNode :: (MonadWidget t m, _) => k -> Dynamic t [(Int, Bool)] -> m (Dynamic t (Int, Bool))
mkNode k ports = do
  levelIn <- forDyn ports $ (foldl' max 0) . fmap fst 
  levelOut <- mapDyn succ levelIn
  let constLabel = "I am #" <> show k <> " @"
  label <- ((constLabel <>) . show) `mapDyn` levelOut
  root <- mapDyn (== 0) levelIn
  stateIn <- mapDyn (and . fmap snd) ports
  rec stateOut <- chooseDyn stateIn manual root
      stateAttr <- mapDyn (bool Map.empty ("class" =: "active")) stateOut
      (bel, _) <- elDynAttr' "button" stateAttr $ do
        dynText label
      manual <- toggle False $ domEvent Click bel
  combineDyn (,) levelOut stateOut

bunchaButtons :: MonadWidget t m => m ()
bunchaButtons = do
  cnt <- divClass "control" counterButton
  let insertNew = fmap newKey cnt
  let bmanip = leftmost [ fmap newKey cnt ]
  nodeIn <- foldDyn ($) Map.empty bmanip
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
