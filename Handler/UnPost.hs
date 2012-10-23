module Handler.UnPost where

import Import
import Filesystem
import Filesystem.Path.CurrentOS
import Control.Monad (when)

postUnPostR :: PostName -> Handler ()
postUnPostR post@(PostName post') = do
    liftIO $ do
        let srcdir = photoRoot </> "topost" </> fromText post'
            destdir = postsRoot </> fromText post'
        x <- isFile $ srcdir </> "TEXT"
        when x $ removeFile $ srcdir </> "TEXT"
        createTree destdir
        listDirectory srcdir >>= mapM_ (\fp -> rename fp $ destdir </> filename fp)
        removeDirectory srcdir
    redirect $ PostR post
