module Handler.Fay where

import Import
import SharedTypes
import qualified Data.Text as T
import Yesod.Fay

onCommand :: CommandHandler App App
onCommand render command =
    case command of
        AddPost date slug r -> do
            liftIO $ addPost $ PostName $ T.pack $ filter (/= '-') date ++ slug
            render r ()
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
            render r ()
