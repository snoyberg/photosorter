module Handler.PostList where

import Import
import Data.List (sort)

getPostListR :: Handler RepHtml
getPostListR = do
    posts <- liftIO getPosts
    defaultLayout $ do
        setTitle "Post list"
        $(widgetFile "post-list")
