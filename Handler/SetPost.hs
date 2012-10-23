module Handler.SetPost where

import Import

postSetPostR :: Handler RepJson
postSetPostR = do
    (f, p) <- runInputPost $ (,)
        <$> (FilePath' <$> ireq textField "file")
        <*> (PostName <$> ireq textField "post")
    runDB $ do
        x <- getBy $ UniqueMedia f
        case x of
            Nothing -> do
                _ <- insert $ Media f (Just p)
                return ()
            Just (Entity key val) -> do
                replace key $ val { mediaPost = Just p }
    jsonToRepJson $ object ["result" .= String "success"]
