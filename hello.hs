{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
import Data.Bool
import Reflex.Dom

main = mainWidget stuff

-- flipflop a b e = do
--   tog <- toggle False e
--   mapDyn (bool a b) tog

flipflop a b e = toggle False e >>= mapDyn (bool a b)

mybutton = do
    rec (bel, _) <- el' "button" $ dynText btext
        let clicked = domEvent Click bel
        btext <- flipflop "unclicked" "Clicked!" clicked
    return ()

stuff = do
    text "Hello, world!"
    text "blah"
    mybutton
