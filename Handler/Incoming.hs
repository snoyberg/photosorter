module Handler.Incoming where

import Import
import Control.Monad (forM, forM_)
import Yesod.Form.Jquery
import Filesystem
import Filesystem.Path.CurrentOS (fromText)

getIncomingR :: FilePath' -> Handler RepHtml
getIncomingR root = do
    files' <- liftIO $ filesForIncoming root
    files <- runDB $ forM files' (\fp -> do
        x <- getBy $ UniqueMedia $ snd fp
        return (fp, x))
    posts <- liftIO getPosts
    y <- getYesod
    defaultLayout $ do
        setTitle "Incoming"
        $(widgetFile "incoming")
        $(fayFile "Incoming")
        addScriptEither $ urlJqueryJs y
  where
    isChosen Nothing _ = False
    isChosen (Just (Entity _ m)) s = mediaPost m == Just s

postIncomingR :: FilePath' -> Handler RepHtml
postIncomingR root = do
    files' <- map snd <$> (liftIO $ filesForIncoming root)
    runDB $ forM_ files' $ \(FilePath' fp) -> do
        x <- getBy $ UniqueMedia $ FilePath' fp
        case x of
            Just (Entity key m) -> do
                case m of
                    Media { mediaPost = Just p }
                        | p == PostName "delete" -> liftIO $ removeFile $ fromText fp
                        | otherwise -> liftIO $ moveToPost (FilePath' fp) p
                    _ -> return ()
                delete key
            Nothing -> return ()
    files'' <- liftIO $ filesForIncoming root
    setMessage "It is so"
    if null files''
        then do
            liftIO $ removePostFolder root
            redirect HomeR
        else redirect $ IncomingR root
