module Handler.Blog where

import Import
import Data.Time (UTCTime)
import Yesod.Auth

entryForm :: AppUserId -> UTCTime -> Form Entry
entryForm uid t = do 
   renderDivs $ Entry
       <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
       <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing
       <*> pure uid
       <*> pure t

getBlogR :: Handler Html
getBlogR = do
    t <- liftIO getCurrentTime
    mauth <- maybeAuth
    userId <- requireAuthId
    entries <- runDB $ selectList [] [Desc EntryPosted]
    (entryWidget, enctype) <- generateFormPost $ entryForm userId t
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        $(widgetFile "blog")


postBlogR :: Handler Html
postBlogR = do
    mauth <- maybeAuth
    userId <- requireAuthId

    t <- liftIO getCurrentTime

    ((res, entryWidget), enctype) <- runFormPost $ entryForm userId t 
    case res of
        FormSuccess newEntry -> do
            entryId <- runDB $ insert $ newEntry {entryPosted = t}
            setMessageI $ MsgEntryCreated $ entryTitle newEntry
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectEntry
            $(widgetFile "blogCorrection")
