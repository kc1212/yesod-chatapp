{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.Logger       (runNoLoggingT)
import           Control.Monad              (liftM)
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Data.Text                  (Text, append, pack)
import           Data.Typeable              (Typeable)
import           Database.Persist.Sqlite
import           Database.Persist           (persistUniqueKeys)
import           Text.Hamlet                (shamlet)
import           Text.Shakespeare.Text      (stext)
import           Yesod
import           Yesod.Auth
import qualified Yesod.Auth.Message         as Msg
import qualified Yesod.Auth.HashDB          as HDB

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text           -- userName
    password Text       -- userPassword
    UniqueUser name
    deriving Typeable
|]

mkYesod "App" [parseRoutes|
    /           HomeR       GET
    /auth       AuthR       Auth    getAuth
    /register   RegisterR   POST    -- TODO make this a dispatch of HashDB
    /admin      AdminR      POST
    /chat       ChatR       GET
|]

adminName :: Text
adminName = "admin"

data App = App
    { sqlBackEnd  :: SqlBackend
    , httpManager :: Manager
    }

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR
    isAuthorized AdminR _   = isAdmin
    isAuthorized ChatR _   = isLoggedIn
    isAuthorized (AuthR LoginR) _ = do
        auth <- isLoggedIn
        case auth of
            Authorized -> return $ Unauthorized "Please logout first."
            otherwise  -> return Authorized
    isAuthorized _ _ = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App conn _ <- getYesod
        runSqlConn f conn

instance YesodAuthPersist App where
    type AuthEntity App = User

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ HDB.authHashDBWithForm loginWidget (Just . UniqueUser) ]
    getAuthId = HDB.getAuthIdHashDB AuthR (Just . UniqueUser)
    authHttpManager = httpManager

instance HDB.HashDBUser User where
    userPasswordHash = Just . userPassword
    setPasswordHash h u = u { userPassword = h }

loginWidget :: Route App -> Widget
loginWidget action = $(whamletFile "loginform.hamlet")

registerSucc :: User -> Handler ()
registerSucc user = do
    succ <- addUserToDB user
    if succ
        then setMessage "Success!, please login." >> redirect (AuthR LoginR)
        else setMessage "User name already exist, please try again." >> redirect HomeR

registerFail :: Handler ()
registerFail = setMessage "Critical Error!" >> redirect HomeR

postRegisterR :: Handler ()
postRegisterR = do
    ((result, widget), enctype) <- runFormPost registerForm
    case result of
        FormSuccess user -> registerSucc user
        _ -> registerFail

getHomeR :: Handler Html
getHomeR = do
    ma <- maybeAuth
    let maUser = liftM (userName . entityVal) ma
    (widget, enctype) <- generateFormPost registerForm
    defaultLayout
        [whamlet|
            $maybe name <- maUser
                <p>Hello #{name}, you're logged in.
                $if name == adminName
                    <form method=post action=@{AdminR}>
                        <button>Delete messages
                        (Admin only)
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Login
                    or register below.
                ^{registerWidget widget enctype}
        |]

getChatR :: Handler Html
getChatR = do
    defaultLayout
        [whamlet| welcome! |]

postAdminR :: Handler ()
postAdminR = do
    setMessage "Done."
    redirect HomeR

registerWidget :: Widget -> Enctype -> Widget
registerWidget w e =
    [whamlet|
        <form method=post action=@{RegisterR} enctype=#{e}>
            ^{w}
            <button>Submit
    |]

registerForm :: Html -> MForm Handler (FormResult User, Widget)
registerForm = renderDivs $ User
    <$> areq textField "Username" Nothing
    <*> areq passwordField "Password" Nothing

addUserToDB :: User -> Handler Bool
addUserToDB user = do
    maybeUser <- runDB $ getBy (UniqueUser (userName user))
    case maybeUser of
        Nothing  -> runDB $ do
                        newUser <- HDB.setPassword (userPassword user) user
                        insert newUser >> return True
        Just _ -> return False

isLoggedIn :: Handler AuthResult
isLoggedIn = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must be logged in."
        otherwise -> Authorized

isAdmin :: Handler AuthResult
isAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just x -> if (userName . entityVal) x == adminName
                    then Authorized
                    else Unauthorized "You must be an admin"

main :: IO ()
main = runNoLoggingT $ withSqliteConn "user.db3" $ \conn -> liftIO $ do
    man <- newManager
    runSqlConn (runMigration migrateAll) conn
    warp 3000 (App conn man)



