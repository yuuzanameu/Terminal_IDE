{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Db where 

import Data.Password.Bcrypt 
import Database.PostgreSQL.Simple
import Data.Pool ( withResource, defaultPoolConfig, newPool, Pool )
import Data.Text (Text, pack)
import Data.UUID.V4 (nextRandom)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, secondsToNominalDiffTime)
import Control.Concurrent.Async (withAsync, wait)

data EmailStatus 
    = EmailVerified {hashedPassword :: Text}
    | EmailUnverified 
    | EmailUnavailable 
    deriving (Eq, Show)

data TokenStatus 
    = TokenExpired 
    | TokenUnavailable 
    | TokenActive {emailId :: Text} 
    deriving (Eq, Show)

type Email = Text

createDbPool :: IO (Pool Connection)
createDbPool = do 
    print ("setting up db pool" :: String)
    newPool $ defaultPoolConfig connectDb close 60 50 

connectDb :: IO Connection
connectDb = connect defaultConnectInfo
  { connectHost = "localhost"
  , connectUser = "postgres"
  , connectPassword = "1772"
  , connectDatabase = "users"
  , connectPort = 5432
  }

createVerificationToken :: IO Text 
createVerificationToken = pack. show <$> nextRandom 

hashPswd :: Text -> IO Text 
hashPswd pswd = do 
    let password = mkPassword pswd  
    hashed <- withAsync (hashPassword password) $ \a -> do wait a  
    pure . unPasswordHash $ hashed 

-- DATABASE QUERY FUNCTIONS 

checkEmailStatus :: Pool Connection -> Email -> IO EmailStatus 
checkEmailStatus pool email = do
    withResource pool $ \conn -> do 
        rows <- query conn 
                    "select verified, password from userdata where email = ?;" 
                    (Only email) :: IO [(Bool, Text)]
        let res = case rows of 
                    [] -> EmailUnavailable 
                    [(True, pswd)] -> EmailVerified pswd  
                    [(False, _)] -> EmailUnverified 
        pure res 


addUnverifiedUser :: Pool Connection -> Email -> Text -> IO Text  
addUnverifiedUser pool email pswd = do 
    withResource pool $ \conn -> do   
        currentTime <- getCurrentTime
        token <- createVerificationToken
        let expires_at = addUTCTime (secondsToNominalDiffTime (24 * 60 * 60)) currentTime
        hashedPswd <- hashPswd pswd 

        rowsAffected <- execute conn 
              (  "insert into userdata (email, password, verified)" 
              <> "values (?, ?, ?);") 
              (email, hashedPswd, False)  
        rowsAffected' <- execute conn 
              (  "insert into verification_tokens (token, email, expires_at)" 
              <> "values (?, ?, ?);")
              (token, email, expires_at)

        print (rowsAffected, rowsAffected')
        pure token 

checkTokenStatus :: Pool Connection -> String -> IO TokenStatus 
checkTokenStatus pool token = do 
    withResource pool $ \conn -> do 
        nowTime <- getCurrentTime
        res <- query conn 
                    "select email, expires_at from verification_tokens where token = ?;" 
                    (Only token) :: IO [(Text, UTCTime)]
        case res of 
            [] -> pure TokenUnavailable 
            [(email, expires_at)] -> 
                if nowTime > expires_at 
                    then pure TokenExpired 
                    else pure $ TokenActive email 


doPswdMatch :: Text -> Text -> IO Bool  
doPswdMatch pswd pswdHash = do
    let pswdHash' = PasswordHash {unPasswordHash = pswdHash}
    case checkPassword (mkPassword pswd) pswdHash' of 
        PasswordCheckFail -> pure False 
        PasswordCheckSuccess -> pure True 

setEmailVerified :: Pool Connection -> Email -> IO()
setEmailVerified pool email' = do 
    withResource pool $ \conn -> do  
        _ <- execute conn   
                 "update userdata set verified = true where email = ?; "
                (Only email') 
        pure ()


deleteExpiredTokens :: Pool Connection -> String -> IO() 
deleteExpiredTokens pool token = do  
    withResource pool $ \conn -> do 
        _ <- execute conn 
                "delete from userdata where token = ?;"
                (Only token)
        pure ()

