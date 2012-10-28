{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Language.Fay.Prelude
import Language.Fay.FFI

data Returns a = Returns
    deriving (Show, Read, Data, Typeable)

data Command = AddPost String String (Returns ())
             | SetPost String String (Returns ())
    deriving (Show, Read, Data, Typeable)

instance Foreign Command
