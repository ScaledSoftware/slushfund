module Handler.Blog where

import Import
import Yesod.Auth

entryForm :: Form Entry
entryForm = do 
   renderDivs $ Entry
       <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
       <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing
       <*> lift requireAuthId
       <*> lift (liftIO getCurrentTime)

getBlogR :: Handler Html
getBlogR = do
    t <- liftIO getCurrentTime
    mauth <- maybeAuth
    entries <- runDB $ selectList [] [Asc EntryCreated]

    if isJust mauth
        then do
             (entryWidget, enctype) <- generateFormPost $ entryForm
             defaultLayout $ do
                 setTitleI MsgBlogArchiveTitle
                 $(widgetFile "blog")
                 $(widgetFile "blogEntry")
        else defaultLayout $ do
                 setTitleI MsgBlogArchiveTitle
                 $(widgetFile "blog")



postBlogR :: Handler Html
postBlogR = do
    mauth <- maybeAuth

    t <- liftIO getCurrentTime

    ((res, entryWidget), enctype) <- runFormPost $ entryForm
    case res of
        FormSuccess newEntry -> do
            entryId <- runDB $ insert $ newEntry {entryCreated = t}
            setMessageI $ MsgEntryCreated $ entryTitle newEntry
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectEntry
            setMessageI MsgPleaseCorrectEntry
            $(widgetFile "blogCorrection")
