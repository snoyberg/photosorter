{-# LANGUAGE NoImplicitPrelude #-}
module Post where

import Language.Fay.Prelude
import Language.Fay.JQuery
import Language.Fay.DOM
import Language.Fay.FFI
import Language.Fay.Yesod

import SharedTypes

onUpdate :: EventObject -> Fay Bool
onUpdate e = do
    t <- eventSource e
    file <- parent t >>= childrenMatching "input[type=date]" >>= getVal
    newname <- getVal t
    call (SetName file newname) $ const $ return ()
    return True

descUpdate :: EventObject -> Fay Bool
descUpdate e = do
    t <- eventSource e
    post <- getData "post" t
    content <- getVal t
    call (SetDescription post content) $ const $ return ()
    return True

main :: Fay ()
main = ready $ do
    select "input[name=newname]"
        >>= onKeyUp onUpdate
        >>= onKeyDown onUpdate
        >>= onKeyPress onUpdate

    select "#description"
        >>= onKeyUp descUpdate
        >>= onKeyDown descUpdate
        >>= onKeyPress descUpdate

    return ()
