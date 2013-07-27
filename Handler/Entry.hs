module Handler.Entry where

import Import
import Yesod.Auth

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do

    muser <- maybeAuth
    entry <- runDB $ get404 entryId
    comments <- runDB $ selectList [CommentEntry ==. entryId] [Asc CommentCreated]

    if isJust muser
        then do
            -- user is logged in.
            (commentWidget, enctype) <- generateFormPost (commentForm entryId)
            defaultLayout $ do
                setTitleI $ MsgEntryTitle $ entryTitle entry
                $(widgetFile "entry")
                $(widgetFile "entryComment")
        else defaultLayout $ do
                setTitleI $ MsgEntryTitle $ entryTitle entry
                $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    t <- liftIO getCurrentTime
    ((res, commentWidget), enctype) <- runFormPost (commentForm entryId)


    entry <- runDB $ get404 entryId
    comments <- runDB $ selectList [CommentEntry ==. entryId] [Asc CommentCreated]

    muser <- maybeAuth

    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessageI MsgCommentAdded
            redirect $ EntryR entryId

        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectComment
            $(widgetFile "entryComment")
