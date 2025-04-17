{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CommonConfig (AppM, appCookieSettings, appjwtSettings, AppContext(..)) where 

import Servant ( Handler ) 
import Servant.Auth.Server as SAS
    ( readKey,
      defaultCookieSettings,
      defaultJWTSettings,
      CookieSettings(sessionCookieName,cookieMaxAge, cookiePath),
      JWTSettings) 

import Data.Time(secondsToDiffTime)

import Database.PostgreSQL.Simple  (Connection)
import Data.Pool                   (Pool)
import Control.Monad.Trans.Reader  (ReaderT) 


data AppContext = AppContext 
    { pool :: Pool Connection 
    , jwtConfig :: JWTSettings 
    } 

-- type AppM = ReaderT (Pool Connection) Handler
type AppM = ReaderT AppContext Handler

appjwtSettings :: IO JWTSettings
appjwtSettings = do 
    key <- readKey "/etc/servant_jwt/key.key"
    pure $ defaultJWTSettings key  

appCookieSettings :: CookieSettings
appCookieSettings = defaultCookieSettings 
    { sessionCookieName = "session_cookie"
    , cookieMaxAge = Just $ secondsToDiffTime (60 * 10)
    }

