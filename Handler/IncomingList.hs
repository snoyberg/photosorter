module Handler.IncomingList where

import Import
import Data.List (sort)

getIncomingListR :: Handler RepHtml
getIncomingListR = do
    folders <- liftIO incomingFolders
    defaultLayout $ do
        setTitle "Incoming Folders"
        $(widgetFile "incoming-list")
