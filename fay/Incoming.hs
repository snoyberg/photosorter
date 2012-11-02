{-# LANGUAGE NoImplicitPrelude #-}
module Incoming where

import Language.Fay.Prelude
import Language.Fay.JQuery
import Language.Fay.DOM
import Language.Fay.FFI
import Language.Fay.Yesod

import SharedTypes

main :: Fay ()
main = ready $ do
    select "button.add-new-post" >>= onClick (\e -> do
        eventSource e >>= hide >>= next >>= unhide
        return False
        )
    select "button.add-new-post2" >>= onClick (\e -> do
        t <- eventSource e
        date <- parent t >>= childrenMatching "input[type=date]" >>= getVal
        slug <- parent t >>= childrenMatching "input[type=text]" >>= getVal
        call (AddPost date slug) $ const reload
        return False
        )
    select "input[type=radio]" >>= onClick (\e -> do
        t <- eventSource e
        file <- parentsSelector "form" t >>= childrenMatching "input[name=file]" >>= getVal
        post <- getVal t
        call (SetPost file post) $ const $ return ()
        return False
        )
    return ()
