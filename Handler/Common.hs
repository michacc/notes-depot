module Handler.Common where

{- Module that contains functions used by many handlers. -}

import Import
import Prelude
import Yesod.Auth(Route(LogoutR), maybeAuth)

{- Key of the last page number that has been recently seen by the user saved
   in the session -}
lastPageNumberKey :: Text
lastPageNumberKey = "lastPageNumber"

{- A widget that contains login information and an option to log out. It appear
   if and only if a user is currently logged in. -}
loginBox :: Widget
loginBox = do
    maybeUser <- handlerToWidget $ fmap entityVal <$> maybeAuth
    authInfoId <- newIdent
    $(widgetFile "loginBox")

{- If the note with given identifier is associated with given user identifier,
   returns it. Otherwise returns 'permission denied' response. If a note with
   given identifer does not exist, returns 'not found' response. -}
guardUserId :: UserId -> NoteId -> Handler Note
guardUserId userId noteId = do
    maybeNote <- runDB $ get noteId
    note <- case maybeNote of
                Just note' -> return note'
                Nothing -> notFound
    if userId == noteUser note
        then return note
        else permissionDenied "unauthorized access"

{- A widget that consists solely of a form that causes removal of the note with
   given identifier. The button inside can be styled to look like a link. -}
createRemoveLink :: NoteId -> Text -> Widget
createRemoveLink noteId linkText = [whamlet|
        <form .noteRemovalForm method="post" action="@{RemoveNoteR noteId}">
            <button type="submit">#{linkText}
    |]
