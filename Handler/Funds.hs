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
            let myPId = appUserPerson appUser 
            -- use my person Id (myPId) to get all funds for this person
            entityFunds <- runDB $ selectList ([FundP1 ==. myPId] ||. [FundP2 ==. myPId] )
                                        [Asc  FundCreated]
            
            let funds = map (\(Entity fundId f) -> (fundId, f)) entityFunds
            let fundPersonIds = map (fundOtherPerson myPId . snd) funds
            let pid2fid = map (\f -> (fundOtherPerson myPId (snd f), fst f)) funds 
        
            entityPersons <- runDB $ selectList [PersonId <-. fundPersonIds]
                                                [Asc PersonNickName]
            let person2fid = map (\(Entity ePId ePerson) -> 
                     (personNickName ePerson, fromJust $ lookup ePId pid2fid)) entityPersons
        
            defaultLayout $ do
                setTitleI $ MsgAllFundsTitle
                $(widgetFile "funds")
