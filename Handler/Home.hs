{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Data.Maybe
import Control.Monad.Trans.Maybe

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    mAuth <- maybeAuth

    mFunds <- runMaybeT $ do
        entityAuth <- MaybeT $ maybeAuth
        let myPId = appUserPerson $ entityVal entityAuth
        entityFunds <- lift $ runDB $ selectList ([FundP1 ==. myPId] ||. [FundP2 ==. myPId])
                                          [Asc FundCreated]
        let funds = map (\(Entity fId f) -> (fId, f)) entityFunds
        let pid2Fund = map (\f -> (fundOtherPerson myPId $ snd f, fst f)) funds
        entityPersons <- lift $ runDB $ selectList [PersonId <-. (map fst pid2Fund)]
                                            [Asc PersonNickName]
        let nickname2fid = map (\(Entity pid person) ->
                            (personNickName person, fromJust $ lookup pid pid2Fund)) entityPersons

        return nickname2fid

    entries <- runDB $ selectList [] [Asc EntryCreated, LimitTo 1]


    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgWelcomeHomepage
        $(widgetFile "homepage") 
