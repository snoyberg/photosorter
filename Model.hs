module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Text.Blaze.Html (ToMarkup)

newtype FilePath' = FilePath' Text
    deriving (Show, Eq, Read, Ord, PathPiece, PersistField, ToMarkup)

newtype PostName = PostName Text
    deriving (Show, Eq, Read, Ord, PathPiece, PersistField, ToMarkup)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
