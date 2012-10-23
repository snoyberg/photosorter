module Handler.SetDescription where

import Import

postSetDescriptionR :: Handler RepJson
postSetDescriptionR = do
    (p, c) <- runInputPost $ (,) <$> (PostName <$> ireq textField "post") <*> ireq textareaField "content"
    runDB $ do
        x <- getBy $ UniqueDescription p
        case x of
            Nothing -> do
                _ <- insert $ Description p c
                return ()
            Just (Entity key val) -> replace key val { descriptionContent = c }
    jsonToRepJson $ object ["result" .= String "success"]
