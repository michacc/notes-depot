{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SimpleAuth
    ( authSimple
    , Username(..)
    , Password(..)
    , PasswordHash(..)
    , YesodAuthSimple(..)
    ) where

import Control.Applicative((<*>))
import Crypto.Hash.SHA256(hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char(intToDigit)
import Data.Functor ((<$>))
import Data.Text as T(Text, append, length, pack, unpack)
import Prelude
import Yesod
import Yesod.Auth

authSimple :: YesodAuthSimple site => AuthPlugin site
authSimple = AuthPlugin pluginName dispatch loginWidget
    where dispatch "POST" ["login"] = postLoginR >>= sendResponse
          dispatch "GET" ["register"] = getRegisterR >>= sendResponse
          dispatch "POST" ["register"] = postRegisterR >>= sendResponse
          dispatch _ _ = notFound
          loginWidget authToMaster = do
              (widget, enctype) <- generateFormPost loginForm
              authBodyWidget headerLogin $ renderLoginForm NoMsg widget authToMaster enctype

-- Types introduced to reduce semantic errors
newtype Username = Username Text deriving (Show, Eq)
newtype Password = Password Text deriving (Show, Eq)
newtype PasswordHash = PasswordHash Text deriving (Show, Eq)

-- Type that represents data used for the user authentication
data AuthenticationData = AuthenticationData
    { adUsername :: Username
    , adPassword :: Password
    }

-- Type with data provided during the registration
data RegistrationData = RegistrationData
    { rdUsername :: Username
    , rdPassword1 :: Password
    , rdPassword2 :: Password
    }

-- Type that is used to carry a message about the form action result
data ResultMsg
    = SuccessMsg Text
    | ErrorMsg Text
    | NoMsg

{- Class that the application must be an instance of to use this authentication
   plugin -}
class YesodAuth site => YesodAuthSimple site where
    addUser :: Username -> PasswordHash -> HandlerT site IO Bool
    getPasswordHash :: Username -> HandlerT site IO (Maybe PasswordHash)

passwordHash :: Password -> PasswordHash
passwordHash (Password password) = PasswordHash $ compute password
    where compute = pack . hex . BS.unpack . hash . BS8.pack . unpack
          hex = concatMap hexByte
          hexByte w8 =
              let hexBase = 16
                  (l1, l2) = (w8 `div` hexBase, w8 `mod` hexBase)
                  convert = intToDigit . fromInteger . toInteger
              in [convert l1, convert l2]

pluginName :: Text
pluginName = "simple"

headerLogin :: Text
headerLogin = "Logowanie"

headerRegister :: Text
headerRegister = "Rejestracja"

piecesRegisterR :: [Text]
piecesRegisterR = ["register"]

piecesLoginR :: [Text]
piecesLoginR = ["login"]

registerR :: Route Auth
registerR = PluginR pluginName piecesRegisterR

loginR :: Route Auth
loginR = PluginR pluginName piecesLoginR

authBodyWidget :: (MonadWidget m, ToWidget s a, HandlerSite m ~ s) => Text -> a -> m ()
authBodyWidget header content= toWidget [whamlet|
        <h1>#{header}
        ^{content}
    |]

loginForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m) =>
    Html -> MForm m (FormResult AuthenticationData, WidgetT (HandlerSite m) IO ())
loginForm = renderDivs $ AuthenticationData
    <$> (Username <$> areq textField "Nazwa użytkownika:" Nothing)
    <*> (Password <$> areq passwordFieldSafe "Hasło:" Nothing)

usernameField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
usernameField = checkBool (isValid . unpack) errMsg textField
    where errMsg :: Text
          errMsg = "Tylko litery, cyfry, znaki podkreślenia i myślniki są "
                   `append` "dozwolone w nazwie użytkownika."
          isValid = all . flip elem $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_"

passwordFieldRegister :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
passwordFieldRegister = checkBool ((>3) . T.length) errMsg passwordFieldSafe
    where errMsg :: Text
          errMsg = "Hasło musi składać się z co najmniej czterech znaków."

-- New field for the password not to show it after form submission
passwordFieldSafe :: Monad m => Field m Text
passwordFieldSafe = Field
    { fieldParse = parse
    , fieldView = view
    , fieldEnctype = UrlEncoded
    }
    where parse [] _ = return $ Right Nothing
          parse [password] _ = return $ Right $ Just password
          parse _ _ = return $ Left "Nieprawidłowe dane."
          view idAttr nameAttr otherAttrs _ isReq = [whamlet|
              $if isReq
                  <input type=password ##{idAttr} name=#{nameAttr} *{otherAttrs} required>
              $else
                  <input type=password ##{idAttr} name=#{nameAttr} *{otherAttrs}>
          |]

registerForm :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m) =>
    Html -> MForm m (FormResult RegistrationData, WidgetT (HandlerSite m) IO ())
registerForm = renderDivs $ RegistrationData
    <$> (Username <$> areq usernameField "Nazwa użytkownika:" Nothing)
    <*> (Password <$> areq passwordFieldRegister "Hasło:" Nothing)
    <*> (Password <$> areq passwordFieldRegister "Powtórz hasło:" Nothing)

renderFormGeneric :: (Yesod s, ToWidget s w) =>
    ResultMsg -> w -> Route s -> Enctype -> Text -> WidgetT s IO ()
renderFormGeneric resultMsg widget destRoute enctype submitText = [whamlet|
        $case resultMsg
            $of SuccessMsg successMsg
                <p .success>#{successMsg}
            $of ErrorMsg errMsg
                <p .error>#{errMsg}
            $of NoMsg
        <form method=post enctype=#{enctype} action="@{destRoute}">
            ^{widget}
            <p><input type="submit" value="#{submitText}">
    |]

renderLoginForm :: (Yesod s, ToWidget s w) =>
    ResultMsg -> w -> (Route Auth -> Route s) -> Enctype -> WidgetT s IO ()
renderLoginForm resultMsg widget authToMaster enctype = do
    let destRoute = authToMaster loginR
    renderFormGeneric resultMsg widget destRoute enctype "Zaloguj się"
    [whamlet|
        <p><a href="@{authToMaster registerR}">Zarejestruj się</a>
    |]

renderRegisterForm :: (Yesod s, ToWidget s w) =>
    ResultMsg -> w -> (Route Auth -> Route s) -> Enctype -> WidgetT s IO ()
renderRegisterForm resultMsg widget authToMaster enctype = do
    let destRoute = authToMaster registerR
    renderFormGeneric resultMsg widget destRoute enctype "Zarejestruj się"
    [whamlet|
        <p><a href="@{authToMaster LoginR}">Powrót do logowania</a>
    |]

postLoginR :: YesodAuthSimple site => AuthHandler site TypedContent
postLoginR = do
    ((result, widget), enctype) <- lift $ runFormPost loginForm
    parentRoute <- getRouteToParent
    let invalidData addMsg = lift . fmap toTypedContent . authLayout $
            let msg =
                    if addMsg
                        then ErrorMsg "Podane dane są nieprawidłowe."
                        else NoMsg
            in authBodyWidget headerLogin $ renderLoginForm msg widget parentRoute enctype
    case result of
        FormSuccess authData -> do
            let username@(Username rawUsername) = adUsername authData
            maybeHash <- lift $ getPasswordHash username
            case maybeHash of
                Just hashPattern -> do
                    let hashSubmitted = passwordHash $ adPassword authData
                    if hashSubmitted == hashPattern
                        then lift $ setCredsRedirect $ Creds pluginName rawUsername []
                        else invalidData True
                Nothing -> invalidData True
        FormMissing -> invalidArgs []
        FormFailure _ -> invalidData False

getRegisterR :: YesodAuthSimple site => AuthHandler site Html
getRegisterR = do
    (widget, enctype) <- lift $ generateFormPost registerForm
    parentRoute <- getRouteToParent
    let formWidget = renderRegisterForm NoMsg widget parentRoute enctype
    lift $ authLayout $ authBodyWidget headerRegister formWidget

postRegisterR :: YesodAuthSimple site => AuthHandler site TypedContent
postRegisterR = do
    ((formResult, widget), enctype) <- lift $ runFormPost registerForm
    parentRoute <- getRouteToParent
    let response = lift . fmap toTypedContent . authLayout . authBodyWidget headerRegister
        formResponse msg = response $ renderRegisterForm msg widget parentRoute enctype
        successResponse = response [whamlet|
            <p>
                Rejestracja zakończyła się sukcesem.
                Teraz możesz <a href="@{parentRoute LoginR}">zalogować się</a>.
        |]
    case formResult of
        FormSuccess regData -> do
            let (password, password2) = (rdPassword1 regData, rdPassword2 regData)
            if password == password2
                then do
                    let username = rdUsername regData
                    addResult <- lift $ addUser username $ passwordHash password
                    if addResult
                        then successResponse
                        else formResponse $ ErrorMsg "Użytkownik o podanej nazwie już istnieje."
                else formResponse $ ErrorMsg "Podane hasła różnią się."
        FormMissing -> invalidArgs []
        FormFailure _ -> formResponse NoMsg
