module Handler.Command where

import Import
import SharedTypes
import Data.Aeson (decode)
import Language.Fay.Convert
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

type HandleCommand = forall s. (forall a. Show a => Returns a -> a -> Handler s) -> Command -> Handler s

handle :: HandleCommand -> Handler RepJson
handle f = do
    mtxt <- lookupPostParam "json"
    case mtxt of
        Nothing -> error "No JSON provided"
        Just txt ->
            case decode (L.fromChunks [encodeUtf8 txt]) >>= readFromFay of
                Nothing -> error $ "Unable to parse input: " ++ show txt
                Just cmd -> f go cmd
  where
    go Returns = jsonToRepJson . showToFay

handleCommand :: HandleCommand
handleCommand respond command =
    case command of
        AddPost date slug r -> do
            liftIO $ addPost $ PostName $ T.pack $ filter (/= '-') date ++ slug
            respond r ()
        SetPost f' p' r -> do
            let f = FilePath' $ T.pack f'
                p = PostName $ T.pack p'
            runDB $ do
                x <- getBy $ UniqueMedia f
                case x of
                    Nothing -> do
                        _ <- insert $ Media f (Just p)
                        return ()
                    Just (Entity key val) -> do
                        replace key $ val { mediaPost = Just p }
            respond r ()

postCommandR :: Handler RepJson
postCommandR = handle handleCommand
