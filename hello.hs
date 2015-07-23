{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
import Reflex.Dom

main = mainWidget stuff

flipflop e a b = 

mybutton = do
    rec (bel, _) <- el' "button" $ dynText btext
        let clicked = domEvent Click bel
        btext <- holdDyn "unclicked" $ tag (constant "Clicked!") clicked
    return ()

stuff = do
    text "Hello, world!"
    text "blah"
    mybutton
