module Handler.ToPostList where

import Import
import Data.List (sort)

getToPostListR :: Handler RepHtml
getToPostListR = do
    toposts <- liftIO getToPosts
    defaultLayout $ do
        setTitle "To post list"
        $(widgetFile "topost-list")
