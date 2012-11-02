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
        SetDescription post' content' r -> do
            let post = PostName $ T.pack post'
                content = Textarea $ T.pack content'
            runDB $ do
                x <- getBy $ UniqueDescription post
                case x of
                    Nothing -> do
                        _ <- insert $ Description post content
                        return ()
                    Just (Entity key val) -> replace key val { descriptionContent = content }
            render r ()
        SetName fp' newname' r -> do
            let fp = FilePath' $ T.pack fp'
                mnewname
                    | null newname' = Nothing
                    | otherwise = Just $ T.pack newname'
            runDB $ do
                case mnewname of
                    Just newname -> do
                        x <- getBy $ UniqueName fp
                        case x of
                            Nothing -> do
                                _ <- insert $ Name fp newname
                                return ()
                            Just (Entity key val) -> replace key val { nameName = newname }
                    Nothing -> deleteBy $ UniqueName fp
            render r ()
