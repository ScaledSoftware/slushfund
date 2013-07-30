module Handler.CreateFund where

import Import

postCreateFundR :: PersonId -> Handler Html
postCreateFundR otherPId = do
    Entity _ appUser <- requireAuth
    let userPId = appUserPerson appUser

    if otherPId == userPId
      then do
          setMessageI MsgCannotCreateFundWSelf
          redirect FundsR
      else do
          maybeFund <- runDB $ getBy $ UniqueFund (min userPId otherPId) (max userPId otherPId)
          case maybeFund of
              Just (Entity fundId _) -> redirect $ FundR fundId
              Nothing -> do
                  currTime <- liftIO getCurrentTime        
                  fundP <- runDB $ get404 otherPId        
                  let fund = Fund (min userPId otherPId)    -- p1        
                                  (max userPId otherPId)    -- p2        
                                  (personNickName fundP)    -- nickname        
                                  currTime                  -- created time        
                
                  fundId <- runDB $ insert fund        
                  redirect $ FundR fundId        
                  

