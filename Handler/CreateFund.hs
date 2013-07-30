module Handler.CreateFund where

import Import

postCreateFundR :: PersonId -> Handler Html
postCreateFundR fundPId = do
    Entity _ appUser <- requireAuth
    let userPId = appUserPerson appUser

    if fundPId == userPId
      then do
          setMessageI MsgCannotCreateFundWSelf
          redirect FundsR
      else do
          currTime <- liftIO getCurrentTime
          fundP <- runDB $ get404 fundPId
          let fund = Fund (if userPId < fundPId then userPId else fundPId)  --p1
                          (if userPId > fundPId then userPId else fundPId)  --p2
                          (personNickName fundP)
                          currTime

          fundId <- runDB $ insert fund
          redirect $ FundR fundId
          

