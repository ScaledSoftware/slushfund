module Handler.AddFundEntry where

import Import

fundEntryForm :: FundId -> [(Text, PersonId)] -> Form FundEntry
fundEntryForm fundId peopleList = renderDivs $ FundEntry
    <$> pure fundId

        -- payer
    <*> areq (selectFieldList peopleList) (fieldSettingsLabel MsgSelectPayer) Nothing

        --amount
    <*> areq doubleField (fieldSettingsLabel MsgAmount) Nothing

        -- wasSplit
    <*> areq boolField (fieldSettingsLabel MsgSplitCheck) Nothing

        -- where
    <*> areq textField (fieldSettingsLabel MsgWhereDidTransactionOccur) Nothing

        -- notes
    <*> areq textField (fieldSettingsLabel MsgNotes) Nothing

        -- what day was the transaction.
    <*> areq dayField (fieldSettingsLabel MsgDate) Nothing

        -- what time was the transaction.
    <*> areq timeField (fieldSettingsLabel MsgTime) Nothing

        -- when was the record created
    <*> lift (liftIO getCurrentTime)


people fundId = runDB $ do
    fund <- get404 fundId
    p1 <- get404 $ fundP1 fund
    p2 <- get404 $ fundP2 fund

    return [(personNickName p1, fundP1 fund), (personNickName p2, fundP2 fund)]


postAddFundEntryR :: FundId -> Handler Html
postAddFundEntryR fundId = do
    peopleList <- people fundId
    ((res, fundEntryWidget), enctype) <- runFormPost (fundEntryForm fundId peopleList)

    case res of
       FormSuccess fundEntry -> do
           _ <- runDB $ insert fundEntry
           redirect $ FundR fundId
       _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectFundEntry
            $(widgetFile "addFundEntry")
