module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile, FilePath)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import Filesystem
import Filesystem.Path.CurrentOS
import Control.Monad.Trans.Writer
import Control.Monad (foldM, filterM)
import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

photoRoot :: FilePath
photoRoot = "/home/ubuntu/Desktop/photos"

incomingRoot :: FilePath
incomingRoot = photoRoot </> "incoming"

postsRoot :: FilePath
postsRoot = photoRoot </> "posts"

toFilePath' :: FilePath -> FilePath'
toFilePath' = FilePath' . either id id . toText

getPosts :: IO [PostName]
getPosts = do
    createTree postsRoot
    fmap (sort . map (PostName . either id id . toText . filename)) $ listDirectory postsRoot >>= filterM isDirectory

getToPosts :: IO [PostName]
getToPosts = do
    createTree $ photoRoot </> "topost"
    fmap (sort . map (PostName . either id id . toText . filename)) $ listDirectory (photoRoot </> "topost") >>= filterM isDirectory

addPost :: PostName -> IO ()
addPost (PostName x) = createTree $ postsRoot </> fromText x

removePostFolder :: FilePath' -> IO ()
removePostFolder (FilePath' fp) = removeDirectory $ fromText fp

moveToPost :: FilePath' -> PostName -> IO ()
moveToPost (FilePath' fp') (PostName post) = do
    let src = fromText fp'
        dest = postsRoot </> fromText post </> filename src
    createTree $ directory dest
    rename src dest

incomingFolders :: IO [FilePath']
incomingFolders = do
    createTree incomingRoot
    fmap (map toFilePath' . ($ [])) $ execWriterT $ loop incomingRoot
  where
    loop fp = do
        fileCount <- liftIO (listDirectory fp) >>= foldM go 0
        if fileCount > (0 :: Int)
            then tell (fp:)
            else return ()
    go fileCount fp = do
        x <- liftIO $ isDirectory fp
        if x
            then do
                loop fp
                return fileCount
            else do
                y <- liftIO $ isFile fp
                return $! fileCount + (if y then 1 else 0)

filesForIncoming :: FilePath' -> IO [(FilePath', FilePath')]
filesForIncoming (FilePath' fp) =
    fmap (map (toFilePath' . filename . fst &&& toFilePath' . fst) . sortBy (comparing snd))
    $ listDirectory (fromText fp)
    >>= filterM isFile
    >>= mapM (\fp -> fmap ((,) fp) $ getModified fp)

filesForPost :: PostName -> IO [(FilePath', FilePath')]
filesForPost (PostName p) =
    fmap (map (toFilePath' . filename . fst &&& toFilePath' . fst) . sortBy (comparing snd))
    $ listDirectory (postsRoot </> fromText p)
    >>= filterM isFile
    >>= mapM (\fp -> fmap ((,) fp) $ getModified fp)

renameImage :: FilePath' -> Text -> IO ()
renameImage (FilePath' fp') name = do
    let src = fromText fp'
        dest = addExtensions (directory src </> fromText name) (extensions src)
    rename src dest
