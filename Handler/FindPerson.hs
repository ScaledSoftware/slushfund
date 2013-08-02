module Handler.FindPerson where

import Import

data FindFormValueHolder = FindFormValueHolder 
                              { emailEntry :: Text
                              , phoneEntry :: Text
                              }

findForm :: Form FindFormValueHolder
findForm = do
   renderDivs $ FindFormValueHolder
       <$> areq textField (fieldSettingsLabel MsgEmailAddressTitle) Nothing
       <*> areq textField (fieldSettingsLabel MsgPhoneNumTitle) Nothing


getFindPersonR :: Handler Html
getFindPersonR = do
    auth <- requireAuth

    let displayPeopleRecs = []

    (findWidget, enctype) <- generateFormPost findForm
    defaultLayout $ do
        setTitleI MsgFindPerson
        $(widgetFile "findPerson")


postFindPersonR :: Handler Html
postFindPersonR = do
    Entity appUserId appUser <- requireAuth

    ((res, findWidget), enctype) <- runFormPost $ findForm
    case res of
        FormSuccess findFormData -> do
            -- the abbreviation Ent referes to (Entity id val) 
            matchingEmailEnts <- runDB $ selectList [EmailEmail ==. (emailEntry findFormData)] []
            let emailLookup = map (\(Entity _ emailRec) -> ((emailPerson emailRec), (emailRec))) matchingEmailEnts
            let emailPeopleIds = map fst emailLookup

            matchingPhoneEnts <- runDB $ selectList [PhoneNumber ==. (phoneEntry findFormData)] []
            let phoneLookup = map (\(Entity _ phoneRec) -> ((phonePerson phoneRec, phoneRec))) matchingPhoneEnts
            let phonePeopleIds = map (phonePerson . entityVal) matchingPhoneEnts

            let allPeopleIds = filter (/= (appUserPerson appUser)) emailPeopleIds ++ phonePeopleIds
            peopleEnts <- runDB $ selectList [PersonId <-. allPeopleIds] [Asc PersonNickName]

            let displayPeopleRecs = map (\(Entity personId personRec) ->
                                            ( personId
                                            , personRec
                                            , lookup personId emailLookup
                                            , lookup personId phoneLookup
                                            )) peopleEnts

            defaultLayout $ do
                setTitleI MsgFindPerson
                $(widgetFile "findPerson")


        _ -> do
            let displayPeopleRecs = []
            
            defaultLayout $ do
                setTitleI MsgPleaseCorrectEntry
                setMessageI MsgPleaseCorrectEntry
                $(widgetFile "findPerson")
