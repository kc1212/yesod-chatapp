{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
import           Control.Monad.Logger           (runNoLoggingT)
import           Control.Monad                  (forever)
import           Control.Concurrent.STM
import           Conduit                        (($$), mapM_C)
import           Data.Aeson
import           Data.Text                      (Text, pack)
import           Data.Text.Lazy                 (toStrict, fromStrict)
import           Data.Typeable                  (Typeable)
import           Database.Persist.Sqlite
import           GHC.Generics
import           Network.HTTP.Client.Conduit    (Manager, newManager)
import           System.Environment             (getEnv)
import           Text.Blaze.Html.Renderer.Text  (renderHtml)
import           Text.Markdown
import           Text.Hamlet                    (hamletFile)
import           Text.Julius                    (juliusFile)
import           Text.Lucius                    (luciusFile)
import           Yesod
import           Yesod.Auth
import qualified Yesod.Auth.HashDB              as HDB
import           Yesod.WebSockets

data WsMsg = WsMsg
    { msgName :: Text
    , msgContent :: Text
    , msgClear :: Bool
    } deriving (Generic, Show)

instance ToJSON WsMsg
instance FromJSON WsMsg

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

data App = App
    { envApproot  :: Text
    , sqlBackEnd  :: SqlBackend
    , httpManager :: Manager
    , chatChannel :: TChan Text
    }

instance Yesod App where
    approot = ApprootMaster envApproot
    authRoute _ = Just $ AuthR LoginR
    isAuthorized AdminR _   = isAdmin
    isAuthorized ChatR _   = isLoggedIn
    isAuthorized (AuthR LoginR) _ = isNotLoggedIn
    isAuthorized _ _ = return Authorized
    defaultLayout = layout
        where layout widget = do
                ma <- maybeAuth
                let mName = (userName . entityVal) <$> ma
                pc <- widgetToPageContent widget
                mmsg <- getMessage
                withUrlRenderer $(hamletFile "layout.hamlet")

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App _ conn _ _ <- getYesod
        runSqlConn f conn

instance YesodAuthPersist App where
    type AuthEntity App = User

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ HDB.authHashDBWithForm loginWidget (Just . UniqueUser) ]
                    where loginWidget action = $(whamletFile "loginform.hamlet")
    getAuthId = HDB.getAuthIdHashDB AuthR (Just . UniqueUser)
    authHttpManager = httpManager

instance HDB.HashDBUser User where
    userPasswordHash = Just . userPassword
    setPasswordHash h u = u { userPassword = h }

chatHandler :: Text -> WebSocketsT Handler ()
chatHandler name = do
    App _ _ _ writeChan <- getYesod
    readChan <- liftAtomically $ dupTChan writeChan
    race_
        (forever $ liftAtomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (liftAtomically . writeTChan writeChan . buildMsg name))

registerSucc :: User -> Handler () -- Post/Redirect/Get
registerSucc user = do
    isOk <- addUserToDB user
    if isOk
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
    let mName = (userName . entityVal) <$> ma
    (widget, enctype) <- generateFormPost registerForm
    defaultLayout $(whamletFile "home.hamlet")

getChatR :: Handler Html
getChatR = do
    name <- requireAuth
    webSockets (chatHandler $ nameFromEntity name)
    defaultLayout $ do
        toWidget $(whamletFile "chat.hamlet")
        toWidget $(luciusFile "chat.lucius")
        toWidget $(juliusFile "chat.julius")

postAdminR :: Handler ()
postAdminR = do
    App _ _ _ chan <- getYesod
    let msg = toJsonText (WsMsg "" "" True)
    liftAtomically (writeTChan chan msg)
    redirect ChatR

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
                        _ <- insert newUser -- insert gives user ID, not needed here
                        return True
        Just _ -> return False

isLoggedIn :: Handler AuthResult
isLoggedIn = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must be logged in to access this page."
        _       -> Authorized

isNotLoggedIn :: Handler AuthResult
isNotLoggedIn = do
    auth <- isLoggedIn
    return $ case auth of
        Authorized  -> Unauthorized "Please logout first."
        _           -> Authorized

isAdmin :: Handler AuthResult
isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just x -> if keyIsAdmin x
                    then Authorized
                    else Unauthorized "You must be an admin."

main :: IO ()
main = runNoLoggingT $ withSqliteConn "user.db3" $ \conn -> liftIO $ do
    root <- pack <$> getEnv "APPROOT" -- TODO sanity check
    chan <- atomically newBroadcastTChan
    man <- newManager
    runSqlConn (runMigration migrateAll) conn
    warp 3000 (App root conn man chan)


-- helper functions
keyIsAdmin :: UserId -> Bool
keyIsAdmin x = fromSqlKey x == 1

nameFromEntity :: Entity User -> Text
nameFromEntity = userName . entityVal

liftAtomically :: MonadIO m => STM a -> m a
liftAtomically = liftIO . atomically

buildMsg :: Text -> Text -> Text
buildMsg name content =
    toJsonText (WsMsg (f name) (f content) False)
        where f = toStrict
                  . renderHtml
                  . markdown def
                  . fromStrict

