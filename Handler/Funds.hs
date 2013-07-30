module Handler.Funds where

import Import

getFundsR :: Handler Html
getFundsR = do
    mNewUser <- lookupSession "newuser"
    case mNewUser of
        Just _ -> do
            setUltDestCurrent
            setMessage "Please update your profile"
            redirect NewUserSearchR
        Nothing -> do
            -- get the login entity (AppUser)
            Entity _ appUser <- requireAuth
            -- get the personId out of the appuser
            let personId = appUserPerson appUser  -- TODO: from Just is no-no
            -- use the PersonId to get all funds for this person
            entityFunds <- runDB $ selectList ([FundP1 ==. personId] ||. [FundP2 ==. personId] )
                                        [Asc  FundCreated]
            
            -- we just want the funds, we don't care about the FundIds.
            let funds = map (\(Entity fundId f) -> (fundId, f)) entityFunds
            let fundPersonIds = map (fundOtherPerson personId . snd) funds
            let pid2fid = map (\f -> (fundOtherPerson personId (snd f), fst f)) funds 
        
            entityPersons <- runDB $ selectList [PersonId <-. fundPersonIds]
                                                [Asc PersonNickName]
            let person2fid = map (\(Entity ePId ePerson) -> 
                     (personNickName ePerson, fromJust $ lookup ePId pid2fid)) entityPersons
        
            defaultLayout $ do
                setTitleI $ MsgAllFundsTitle
                $(widgetFile "funds")
