module Handler.NotesList
    ( getNotesListR
    , getNoteR
    , getSearchR
    , getSearchGatewayR) where

import Handler.Common
import Import
import qualified Data.Text as T(append, concat, null, pack, take, Text)
import Yesod.Auth(requireAuthId)

notePreviewLength :: Int
notePreviewLength = 200

notesPerPage :: Int
notesPerPage = 10

searchInputName :: T.Text
searchInputName = "phrase"

render :: Text -> Text -> Maybe Text -> [Entity Note] -> Int -> PageNumber ->
          (PageNumber -> Route App) -> Handler Html
render title headerText maybeSearchPhrase entities allEntitiesCount pageNumber
           destRoute = defaultLayout $ do
    setTitle $ toHtml title
    let (allPagesCount, pagesNumbers) = genPagesNumbers allEntitiesCount
        PageNumber rawPageNumber = pageNumber
    if rawPageNumber > allPagesCount
        then redirect $ destRoute $ PageNumber allPagesCount
        else return ()
    setSession lastPageNumberKey $ T.pack . show $ pageNumber
    loginBox
    $(widgetFile "notesList")

showNotes :: [Filter Note] -> Text -> Text -> Maybe Text -> PageNumber ->
             (PageNumber -> Route App) -> Handler Html
showNotes filters title headerText maybeSearchPhrase pageNumber destRoute = do
    userId <- requireAuthId
    let allFilters = (NoteUser ==. userId) : filters
    notesCount <- runDB $ count allFilters
    let (limit, offset) = computePagePosition pageNumber
        selectOptions = [Desc NoteCreated, LimitTo limit, OffsetBy offset]
    entities <- runDB $ selectList allFilters selectOptions
    render title headerText maybeSearchPhrase entities notesCount pageNumber destRoute

getNotesListR :: PageNumber -> Handler Html
getNotesListR pageNumber = do
    let title = "NotesDepot - moje notatki"
        headerText = "Moje notatki"
    showNotes [] title headerText Nothing pageNumber NotesListR

getSearchR :: URLEncodedText -> PageNumber -> Handler Html
getSearchR p@(URLEncodedText searchPhrase) pageNumber = do
    if T.null searchPhrase
        then redirect $ NotesListR $ PageNumber 1
        else return ()
    let title = "NotesDepot - wyniki wyszukiwania"
        headerText = "Wyniki wyszukiwania"
        likeOp = BackendSpecificFilter "like"
        condText = T.concat ["%", searchPhrase, "%"]
        contains field value = Filter field (Left value) likeOp
        titleFilter = contains NoteTitle $ Just condText
        contentFilter = contains NoteContent condText
        filters = [titleFilter] ||. [contentFilter]
        dest = SearchR p
    showNotes filters title headerText (Just searchPhrase) pageNumber dest

getSearchGatewayR :: Handler Html
getSearchGatewayR = do
    maybeSearchPhrase <- lookupGetParam searchInputName
    case maybeSearchPhrase of
        Just phrase -> redirect $ SearchR (URLEncodedText phrase) $ PageNumber 1
        Nothing ->
            let msg :: [Text]
                msg = ["missing GET data"]
            in invalidArgsI msg

getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    userId <- requireAuthId
    note <- guardUserId userId noteId
    let (Note maybeTitle content created _) = note
    defaultLayout $ do
        setTitle $ toHtml $ "NotesDepot - " `T.append`
                                case noteTitle note of
                                    Just title -> title
                                    Nothing -> "notatka"
        loginBox
        $(widgetFile "note")

{- Returns the partition of notes into pages. Tuple elements:
      1. all pages count
      2. list with numbers of subsequent pages -}
genPagesNumbers :: Int -> (Int, [Int])
genPagesNumbers notesCount
    | notesCount < 1 = (1, [1])
    | otherwise =
        let fullPagesCount = notesCount `div` notesPerPage
            remainingNotesCount = notesCount `mod` notesPerPage
            allPagesCount = fullPagesCount +
                (if remainingNotesCount > 0
                    then 1
                    else 0)
        in (allPagesCount, [1..allPagesCount])

{- Returns the position of the page in the query result. Tuple elements:
       1. number of notes to return from the database
       2. offset of the first note of the page -}
computePagePosition :: PageNumber -> (Int, Int)
computePagePosition (PageNumber pageNumber) =
    (notesPerPage, notesPerPage * (pageNumber - 1))
