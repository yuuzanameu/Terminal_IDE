{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE ViewPatterns #-}

module App where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Db
import JsonTypes
import Mailing
import Servant
import Servant.Auth.Server as SAS
import Servant.Server.Experimental.Auth as Exp

import Data.Text (Text)

-- import Data.Text.Encoding

import Data.Function ((&))
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)

import CommonConfig (AppContext (..), AppM, appCookieSettings)
import Data.Aeson (encode)
import Network.Wai (Request (requestHeaders))
import VerifyEmail
import Web.Cookie (parseCookies)

type MainAPI = HomePage :<|> Signup :<|> VerifyEmail :<|> Login

server :: ServerT MainAPI AppM
server = returnUserData :<|> signup :<|> verifyEmail :<|> login

app :: Pool Connection -> JWTSettings -> IO Application
app pool jwt_settings = do
    let ctx = cookieAuthHandler jwt_settings :. jwt_settings :. defaultCookieSettings :. EmptyContext
    pure $ serveWithContextT (Proxy :: Proxy MainAPI) ctx nt server
  where
    nt :: AppM a -> Handler a
    nt appM = runReaderT appM (AppContext{jwtConfig = jwt_settings, pool = pool})

type instance AuthServerData (AuthProtect "cookie-auth") = Session

type instance AuthServerData (AuthProtect "access-token") = Session

-- throw401 err = throwError err401 {errBody = err}

errorThrow status_code val = throwError status_code{errBody = encode val}

cookieAuthHandler :: JWTSettings -> AuthHandler Request Session
cookieAuthHandler jwt_settings = mkAuthHandler handler
  where
    handler :: Request -> Handler Session
    handler req = do
        cookies <- case lookup "cookie" $ requestHeaders req of
            Nothing -> errorThrow err401 NoCookieInHeader
            Just c -> pure $ parseCookies c
        jwt <- lookup "session_cookie" cookies & maybe (errorThrow err401 NoSessionCookie) pure
        maybeValidSession <- liftIO $ verifyJWT jwt_settings jwt
        session@Session{..} <- maybe (errorThrow err401 InvalidJWT) pure maybeValidSession
        now <- liftIO getCurrentTime
        if now >= expiresAt
            then errorThrow err401 CookieExpired
            else pure session

-- ROUTES AND HANDLERS

type HomePage = AuthProtect "cookie-auth" :> "auth" :> Get '[JSON] LoginResponse

returnUserData :: Session -> AppM LoginResponse
returnUserData Session{..} = pure $ LoggedIn $ Profile{userName = userEmail, profilePic = "J"}

type Signup = "auth" :> "sign-up" :> ReqBody '[JSON] Credentials :> Post '[JSON] Bool

signup :: Credentials -> AppM Bool
signup (Credentials email' pswd) = do
    AppContext{pool} <- ask
    status <- liftIO $ checkEmailStatus pool email'
    case status of
        EmailUnverified -> errorThrow err403 VerificationPending
        EmailVerified _ -> errorThrow err409 AccountAlreadyExists
        EmailUnavailable -> do
            token <- liftIO $ addUnverifiedUser pool email' pswd
            let verificationLink = "http://127.0.0.1/api/auth/verify-email?token=" <> token
            acceptedBySes <- liftIO $ sendEmail email' verificationLink
            if acceptedBySes
                then pure True
                else errorThrow err400 EmailUnreachable

type Login =
    "auth"
        :> "login"
        :> ReqBody '[JSON] Credentials
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)

login :: Credentials -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
login (Credentials email' pswd) = do
    AppContext{pool} <- ask
    status <- liftIO $ checkEmailStatus pool email'
    case status of
        EmailUnavailable -> errorThrow err401 NoSuchEmail
        EmailUnverified -> errorThrow err401 NoSuchEmail
        EmailVerified hashedpswd -> do
            match <- liftIO $ doPswdMatch pswd hashedpswd
            if match
                then liftIO (print $ "They matched: " <> show match) >> sendSessionCookie email'
                else errorThrow err401 PasswordMismatch

sendSessionCookie ::
    Text -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LoginResponse)
sendSessionCookie email' = do
    AppContext{jwtConfig} <- ask
    now <- liftIO getCurrentTime
    let expiry = addUTCTime (secondsToNominalDiffTime $ 60 * 2) now
    let session =
            Session
                { userEmail = email'
                , expiresAt = expiry
                , createdAt = now
                }
    mCookie <- liftIO $ acceptLogin appCookieSettings jwtConfig session
    let prfile = Profile{userName = "Josh_kaizen", profilePic = "J"}
    case mCookie of
        Nothing -> throwError $ err500{errBody = "Internal Server Error: couldnt creake cookie"}
        Just f -> pure $ f $ LoggedIn{userProfile = prfile}
