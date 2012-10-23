module Handler.AddPost where

import Import
import qualified Data.Text as T

postAddPostR :: Handler RepJson
postAddPostR = do
    (d, s) <- runInputPost $ (,) <$> ireq textField "date" <*> ireq textField "slug"
    liftIO $ addPost $ PostName $ T.append (T.filter (/= '-') d) s
    jsonToRepJson $ object ["result" .= String "success"]
