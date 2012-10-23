module Handler.Image where

import Import
import Data.Text (unpack)

getImageR :: FilePath' -> Handler RepHtml
getImageR (FilePath' fp) = sendFile "image/jpeg" $ unpack fp
