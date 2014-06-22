module Handler.NotesCore
    ( getAddNoteR
    , postAddNoteR
    , getEditNoteR
    , postEditNoteR
    , postRemoveNoteR
    ) where

import Control.Monad(join)
import qualified Data.Text as T(null, strip)
import Data.Time.LocalTime(getZonedTime)
import Handler.Common
import Import
import Yesod.Auth(requireAuthId)

-- Type that represents data that user enters in the form to create a note
data NoteData = NoteData
    { ndTitle :: Maybe Text
    , ndContent :: Text
    }

data MidMessage
    = SuccessMsg Text
    | ErrorMsg Text
    | NoMsg

data FormInit
    = InitNoteData NoteData
    | NoInit Widget Enctype
    | InitEmpty

noteForm :: Maybe NoteData -> Form NoteData
noteForm maybeNoteData extra = do
    (resTitle, viewTitle) <- mopt textField "" (ndTitle <$> maybeNoteData)
    (resContent, viewContent) <- mreq textareaField "" (Textarea . ndContent <$> maybeNoteData)
    let result = NoteData <$> resTitle <*> fmap unTextarea resContent
        widget = [whamlet|
            #{extra}
            <div id="noteTitle">Tytuł: ^{fvInput viewTitle}
            <div id="noteContent">Treść:<br>
                ^{fvInput viewContent}
                $maybe errMsg <- fvErrors viewContent
                    <div .errors>#{errMsg}
                $nothing
        |]
    return (result, widget)

{- Removes the given note with beginning and trailing whitespaces removed from
   the note title. -}
cleanTitle :: NoteData -> NoteData
cleanTitle noteData =
    let newTitle oldTitle = do
            stripedTitle <- T.strip <$> oldTitle
            if T.null stripedTitle
                then Nothing
                else return stripedTitle
    in noteData { ndTitle = newTitle $ ndTitle noteData }

render :: Text -> Text -> MidMessage -> Route App -> FormInit -> Text -> Handler Html
render title headerText midMessage destRoute formInit submitText = do
    (widget, enctype) <- getForm formInit
    defaultLayout $ do
        setTitle $ toHtml title
        loginBox
        $(widgetFile "noteForm")
    where getForm (NoInit widget enctype) = return (widget, enctype)
          getForm (InitNoteData noteData) = generateFormPost $ noteForm $ Just noteData
          getForm InitEmpty = generateFormPost $ noteForm Nothing

renderAdd :: MidMessage -> FormInit -> Handler Html
renderAdd midMessage formInit =
    let title = "NotesDepot - dodawanie notatki"
        headerText = "Dodawanie notatki"
        submitText = "Dodaj notatkę"
    in render title headerText midMessage AddNoteR formInit submitText

renderEdit :: NoteId -> MidMessage -> FormInit -> Handler Html
renderEdit noteId midMessage formInit =
    let title = "NotesDepot - edytowanie notatki"
        headerText = "Edytowanie notatki"
        submitText = "Zapisz zmiany"
        destRoute = EditNoteR noteId
    in render title headerText midMessage destRoute formInit submitText

getAddNoteR :: Handler Html
getAddNoteR = renderAdd NoMsg InitEmpty

postAddNoteR :: Handler Html
postAddNoteR = do
    ((formResult, widget), enctype) <- runFormPost $ noteForm Nothing
    case formResult of
        FormSuccess noteData -> do
            userId <- requireAuthId
            curTime <- liftIO getZonedTime
            let noteData' = cleanTitle noteData
            let (title, content) = (ndTitle noteData', ndContent noteData')
            _ <- runDB $ insert $ Note title content curTime userId
            let msg = SuccessMsg "Notatka została pomyślnie utworzona."
            renderAdd msg InitEmpty
        FormFailure _ -> renderAdd NoMsg $ NoInit widget enctype
        FormMissing -> do
            let msg :: [Text]
                msg = ["mising POST data"]
            invalidArgsI msg

getEditNoteR :: NoteId -> Handler Html
getEditNoteR noteId = do
    userId <- requireAuthId
    note <- guardUserId userId noteId
    let noteData = NoteData
            { ndTitle = noteTitle note
            , ndContent = noteContent note
            }
    renderEdit noteId NoMsg (InitNoteData noteData)

postEditNoteR :: NoteId -> Handler Html
postEditNoteR noteId = do
    userId <- requireAuthId
    _ <- guardUserId userId noteId
    ((formResult, widget), enctype) <- runFormPost $ noteForm Nothing
    case formResult of
        FormSuccess noteData -> do
            let noteData' = cleanTitle noteData
            runDB $ update noteId [NoteTitle =. ndTitle noteData', NoteContent =. ndContent noteData']
            let msg = SuccessMsg "Notatka została pomyślnie zapisana."
            renderEdit noteId msg (NoInit widget enctype)
        FormFailure _ -> renderEdit noteId NoMsg $ NoInit widget enctype
        FormMissing -> do
            let msg :: [Text]
                msg = ["missing POST data"]
            invalidArgsI msg

postRemoveNoteR :: NoteId -> Handler Html
postRemoveNoteR noteId = do
    userId <- requireAuthId
    _ <- guardUserId userId noteId
    runDB $ delete noteId
    maybePageNumberText <- lookupSession lastPageNumberKey
    let maybePageNumber = join $ fromPathPiece <$> maybePageNumberText
        pageNumber = case maybePageNumber of
                        Just number -> number
                        Nothing -> PageNumber 1
    redirect $ NotesListR pageNumber
