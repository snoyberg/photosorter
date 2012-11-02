{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Language.Fay.Prelude
import Language.Fay.FFI
import Language.Fay.Yesod

data Command = AddPost String String (Returns ())
             | SetPost String String (Returns ())
    deriving (Show, Read, Data, Typeable)

instance Foreign Command
