module Handler.Fund where

import Import
import Handler.AddFundEntry (fundEntryForm, people)


getFundR :: FundId -> Handler Html
getFundR fundId = do
    fund <- runDB $ get404 fundId
    fundEntries <- runDB $ selectList [FundEntryFund ==. fundId] [Asc FundEntryTransDay]


    Entity _ appUser <- requireAuth
    let myPId = appUserPerson appUser
    me <- runDB $ get404 $ myPId
    let theirPId = fundOtherPerson (appUserPerson appUser) fund
    them <- runDB $ get404 $ theirPId
    let fundNick = personNickName them

    let myDebtRaw = sum $ map (\(Entity _ fe) -> myDebtFromFundEntry myPId fe) fundEntries
    let myDebt = myDebtRaw
    let theirDebt = ((-1.0) * myDebtRaw)
    let evenSteven = myDebt < 1 && theirDebt < 1
    let iOwe = myDebt > 0

    let decoratedFundEntries = map (\(Entity feId fe) -> 
          (feId, fe, if myPId == fundEntryPayer fe then me else them)) fundEntries

    peopleList <- people fundId
    (fundEntryWidget, enctype) <- generateFormPost (fundEntryForm fundId peopleList)


    defaultLayout $ do
        setTitleI $ MsgFundTitle $ fundNick
        $(widgetFile "fund")
        $(widgetFile "addFundEntry")
