module Handler.DeleteFundEntry where

import Import

postDeleteFundEntryR :: FundEntryId -> Handler ()
postDeleteFundEntryR fundEntryId = do
    fundEntry <- runDB $ get404 fundEntryId
    _ <- runDB $ delete fundEntryId
    redirect $ FundR (fundEntryFund fundEntry)
