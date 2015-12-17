{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.Logger (runNoLoggingT)
import           Data.Text                (Text, append, pack)
import           Data.Typeable            (Typeable)
import           Database.Persist.Sqlite
import           Database.Persist         (persistUniqueKeys)
import           Text.Hamlet              (shamlet)
import           Text.Shakespeare.Text    (stext)
import           Yesod
import           Yesod.Auth
import qualified Yesod.Auth.HashDB        as HDB

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text           -- userName
    password Text       -- userPassword
    UniqueUser name
    deriving Typeable
|]


mkYesod "App" [parseRoutes|
    /           HomeR       GET
    /auth       AuthR       Auth getAuth
    /register   RegisterR   POST -- better to be under AuthR
|]

data App = App SqlBackend

instance Yesod App where

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App conn <- getYesod
        runSqlConn f conn

instance YesodAuthPersist App where
    type AuthEntity App = User

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ HDB.authHashDB (Just . UniqueUser) ]
    getAuthId creds = HDB.getAuthIdHashDB AuthR (Just . UniqueUser) creds
    authHttpManager = error "No need an HTTP manager"

instance HDB.HashDBUser User where
    userPasswordHash = Just . userPassword
    setPasswordHash h u = u { userPassword = h }

succHtml :: User -> Handler Html
succHtml user = do
    succ <- addUserToDB user
    if succ
        then defaultLayout
                [whamlet|
                    <p>
                        Success!, now you can
                        <a href=@{AuthR LoginR}>login.
                |]
        else defaultLayout
                [whamlet|
                    <p>
                        User name already exist!, please try
                        <a href=@{RegisterR}>again.
                |]

postRegisterR :: Handler Html
postRegisterR = do
    ((result, widget), enctype) <- runFormPost registerForm
    case result of
        FormSuccess user -> succHtml user
        _ -> defaultLayout
                [whamlet|
                    <p>Error, please try again.
                    ^{registerWidget widget enctype}
                |]

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    (widget, enctype) <- generateFormPost registerForm
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Login
                    or register below.
                ^{registerWidget widget enctype}
        |]

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

main :: IO ()
main = runNoLoggingT $ withSqliteConn "user.db3" $ \conn -> liftIO $ do
    runSqlConn (runMigration migrateAll) conn
    warp 3000 $ App conn



