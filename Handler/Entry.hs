module Handler.Entry where

import Import
import Data.Time
import Yesod.Auth

commentForm :: AppUserId -> UTCTime -> EntryId -> Form Comment
commentForm uid t entryId = renderDivs $ Comment
    <$> pure entryId
    <*> pure t
    <*> pure uid
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    uid <- requireAuthId
    t <- liftIO getCurrentTime
    (commentWidget, enctype) <- generateFormPost (commentForm uid t entryId)

    muser <- maybeAuth

    entries <- runDB $ selectList [] [Desc EntryPosted]
    entry <- runDB $ get404 entryId

    comments <- runDB $ selectList [CommentEntry ==. entryId] [Asc CommentPosted]

    defaultLayout $ do
       setTitleI $ MsgEntryTitle $ entryTitle entry
       $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    uid <- requireAuthId
    t <- liftIO getCurrentTime
    ((res, commentWidget), enctype) <- runFormPost (commentForm uid t entryId)


    entry <- runDB $ get404 entryId
    comments <- runDB $ selectList [CommentEntry ==. entryId] [Asc CommentPosted]

    muser <- maybeAuth

    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessageI MsgCommentAdded
            redirect $ EntryR entryId

        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectComment
            $(widgetFile "entry")
