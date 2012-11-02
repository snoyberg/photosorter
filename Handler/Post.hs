module Handler.Post where

import Import
import Yesod.Form.Jquery
import Control.Monad (forM, forM_)
import qualified Data.Text.IO as TIO
import Filesystem
import Filesystem.Path.CurrentOS

getPostR :: PostName -> Handler RepHtml
getPostR post = do
    mdesc <- runDB $ getBy $ UniqueDescription post
    files' <- liftIO $ filesForPost post
    files <- runDB $ forM files' (\fp -> do
        x <- getBy $ UniqueName $ snd fp
        return (fp, x))
    y <- getYesod
    defaultLayout $ do
        setTitle "Post"
        $(widgetFile "post")
        $(fayFile "Post")
        addScriptEither $ urlJqueryJs y
  where
    getName Nothing = ""
    getName (Just (Entity _ n)) = nameName n

postPostR :: PostName -> Handler ()
postPostR post@(PostName post') = do
    let dir = photoRoot </> "topost" </> fromText post'
    liftIO $ createTree dir
    mdesc <- runDB $ getBy $ UniqueDescription post
    case mdesc of
        Nothing -> return ()
        Just (Entity _ desc) -> liftIO $ TIO.writeFile (encodeString $ dir </> "TEXT") $ unTextarea $ descriptionContent desc
    files' <- liftIO $ filesForPost post
    runDB $ forM_ (map snd files') $ \fp@(FilePath' raw) -> do
        x <- getBy $ UniqueName fp
        let src = fromText raw
        name <-
            case x of
                Nothing -> return $ filename src
                Just (Entity key val) -> do
                    delete key
                    return $ addExtensions (fromText $ nameName val) (extensions src)
        liftIO $ rename src $ dir </> name
    liftIO $ removeDirectory $ postsRoot </> fromText post'
    setMessage "Post moved to topost"
    redirect HomeR
