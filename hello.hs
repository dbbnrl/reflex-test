{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures #-}
import Data.Bool
import Reflex.Dom
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

main :: IO ()
main = mainWidget stuff

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

newKey :: (_) => k -> Map k () -> Map k ()
newKey k = Map.insert k ()

mkButton v = mybutton

bunchaButtons :: MonadWidget t m => m ()
bunchaButtons = do
    cnt <- counterButton
    let insertNew = fmap newKey cnt
    let bmanip = leftmost [ fmap newKey cnt ]
    buttonIn <- foldDyn ($) Map.empty bmanip
    buttonOut <- list buttonIn mkButton
    return ()

stuff :: MonadWidget t m => m ()
stuff = do
    text "Hello, world!"
    text "blah"
    bunchaButtons
    -- mybutton
