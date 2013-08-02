module Handler.Fund where

import Import


getFundR :: FundId -> Handler Html
getFundR fundId = do
    fund <- runDB $ get404 fundId
    let fundNick = fundNickName fund
    fundEntries <- runDB $ selectList [FundEntryFund ==. fundId] [Asc FundEntryTransDate]


    Entity _ appUser <- requireAuth
    let myPId = appUserPerson appUser
    me <- runDB $ get404 $ myPId
    let theirPId = fundOtherPerson (appUserPerson appUser) fund
    them <- runDB $ get404 $ theirPId

    let decoratedFundEntries = map (\(Entity feId fe) -> 
          (feId, fe, if myPId == fundEntryPayer fe then me else them)) fundEntries

    defaultLayout $ do
        setTitleI $ MsgFundTitle $ fundNick
        $(widgetFile "fund")
