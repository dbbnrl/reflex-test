{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, PartialTypeSignatures #-}
import Data.Bool
import Reflex.Dom
import Control.Monad (void)

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

counterButton :: MonadWidget t m => m (Dynamic t Int)
counterButton = do
    rec (bel, _) <- el' "button" $ display cnt
        let clicks = domEvent Click bel
        cnt <- count clicks
    return cnt

-- bunchaButtons :: MonadWidget t m => m ()
-- bunchaButtons = do
--     addEv <- counterButton

stuff :: MonadWidget t m => m ()
stuff = do
    text "Hello, world!"
    text "blah"
    -- bunchaButtons
    mybutton
    void counterButton
