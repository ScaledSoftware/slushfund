module Handler.Person where

import Import

infoForm :: Person -> Form Person
infoForm person = renderDivs $ Person
    <$> areq textField (fieldSettingsLabel MsgFirstName) (Just $ personFirstName person)
    <*> areq textField (fieldSettingsLabel MsgLastName) (Just $ personLastName person)
    <*> areq textField (fieldSettingsLabel MsgNickName) (Just $ personNickName person)
    <*> pure (personCreated person)
    <*> pure (personPicture person)   -- TODO: implement picture upload

getPersonR :: PersonId -> Handler Html
getPersonR personId = do
    _ <- requireAuthId
    dbPerson <- runDB $ get404 personId
    (infowidget, enctype) <- generateFormPost (infoForm dbPerson)

    defaultLayout $ do
        setTitleI $ MsgPersonInfo $ personNickName dbPerson
        $(widgetFile "person")

postPersonR :: PersonId -> Handler Html
postPersonR personId = do
    deleteSession "newuser"
    _ <- requireAuthId
    dbPerson <- runDB $ get404 personId
    ((res, infowidget), enctype) <- runFormPost (infoForm dbPerson)


    case res of
        FormSuccess updatedPerson -> do
            setMessageI $ MsgPersonUpdated $ personNickName updatedPerson
            runDB $ replace personId updatedPerson 
            redirectUltDest FundsR

        _ -> defaultLayout $ do
            setTitleI $ MsgPersonInfo $ personNickName dbPerson
            setMessageI MsgPleaseCorrectPersonEntry
            $(widgetFile "person")
