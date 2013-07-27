module Handler.Fund where

import Import

getFundR :: FundId -> Handler Html
getFundR pid = do
    defaultLayout $ do
        setTitleI $ MsgFundTitle $ " FIXME"
        $(widgetFile "fund")

postFundR :: FundId -> Handler Html
postFundR pid = do
    defaultLayout $ do
        setTitleI $ MsgFundTitle $ " FIXME"
        $(widgetFile "fund")
