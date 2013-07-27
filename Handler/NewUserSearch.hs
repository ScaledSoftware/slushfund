module Handler.NewUserSearch where

import Import

searchForm :: Form (Maybe Text, Maybe Text)
searchForm = do
   renderDivs $ (,)
      <$> aopt textField (fieldSettingsLabel MsgPhoneNumTitle) Nothing
      <*> aopt textField (fieldSettingsLabel MsgEmailAddressTitle) Nothing

getNewUserSearchR :: Handler Html
getNewUserSearchR = do
    Entity appUserId appUser <- requireAuth

    mNewUser <- lookupSession "newuser"
    deleteSession "newuser"
    mEmail <- do
        case mNewUser of
            Just credEmail -> do
                runDB $ getBy $ UniqueEmail $ credEmail
            Nothing -> return Nothing
    
    case mEmail of
        Just (Entity _ email) -> do
            if (emailPerson email == appUserPerson appUser)
               then redirect $ PersonR (emailPerson email)
               else do
                   -- we didn't need to add the Person, one already existed....
                   let oopsPersonId = appUserPerson appUser

                   -- alter the AppUser to point to the original Person
                   
                   runDB $ do
                       update appUserId [AppUserPerson =. (emailPerson email)]
                       delete oopsPersonId

                   redirect $ PersonR (emailPerson email)


        Nothing -> do
            (searchwidget, enctype) <- generateFormPost $ searchForm
            defaultLayout $ do
                setTitleI MsgNewUserSearchTitle
                $(widgetFile "newUserSearch")

postNewUserSearchR :: Handler Html
postNewUserSearchR = error "Not yet implemented: postNewUserSearchR"
