module Handler.SetName where

import Import

postSetNameR :: Handler RepJson
postSetNameR = do
    (fp, mnewname) <- runInputPost $ (,) <$> (FilePath' <$> ireq textField "file") <*> iopt textField "newname"
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
    jsonToRepJson $ object ["result" .= String "success"]
