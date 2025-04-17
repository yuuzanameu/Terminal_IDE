{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

module VerifyEmail (VerifyEmail, verifyEmail) where 

import Servant 
import Servant.HTML.Blaze          (HTML)
import Text.Blaze.Html5            (Html, (!), docTypeHtml, body, h1, p, a)
import Text.Blaze.Html5.Attributes (href)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Reader  (ask) 


import Db 
import CommonConfig 

type VerifyEmail = "auth" :> "verify-email" 
    :> QueryParam "token" String 
    :> Get '[HTML] Html


verifyEmail :: Maybe String -> AppM Html
verifyEmail Nothing = throwError err400 {errBody = "Link is probably broken, sign up from scratch"}
verifyEmail (Just token) = do 
    liftIO $ print ("hello from verifyEmail function" :: String)
    AppContext {pool } <- ask 
    tokenStatus <- liftIO $ checkTokenStatus pool token 
    liftIO $ print tokenStatus
    case tokenStatus of 
        TokenUnavailable -> return linkExpired
        TokenExpired -> do  
            liftIO $ deleteExpiredTokens pool token
            return linkExpired
        TokenActive email' -> do 
            liftIO $ setEmailVerified pool email' 
            return successPage 


linkExpired :: Html
linkExpired = docTypeHtml $ do  
    body $ do 
        h1 "That link has expired or is invalid, try signing in again"

successPage :: Html
successPage = docTypeHtml $ do
    body $ do
        h1 "Email Successfully Verified"
        p $ do
            "Please continue to "
            a ! href "http://127.0.0.1/" $ "and log in"


