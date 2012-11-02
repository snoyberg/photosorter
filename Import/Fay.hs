module Import.Fay
    ( fayFile
    ) where

import Prelude
import Yesod.Fay
import Language.Haskell.TH.Syntax
import Settings.Development (development)

fayFile :: String -> Q Exp
fayFile
    | development = fayFileReload
    | otherwise   = fayFileProd
