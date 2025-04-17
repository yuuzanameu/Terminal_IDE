{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module SendMail where 

import GHC.Generics (Generic)
import Network.HTTP.Client 
    ( httpLbs,
      newManager,
      parseRequest,
      Request(requestHeaders, method, requestBody),
      RequestBody(RequestBodyLBS),
      Response(responseBody) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Data.Aeson (encode, decode, (.=), object, FromJSON, ToJSON)
import Data.Text(Text)

newtype EmailSent = EmailSent {accepted :: Bool} deriving (Generic, Show)

instance FromJSON EmailSent 
instance ToJSON EmailSent 

sendVerificationEmail :: Text -> Text -> IO Bool
sendVerificationEmail email' token = do
    manager <- newManager tlsManagerSettings
    let requestBody' = encode $ object ["email" .= email', "token" .= token]
    initialRequest <- parseRequest "http://localhost:5000/auth/sendmail"
    let request = initialRequest
                  { method = "POST"
                  , requestBody = RequestBodyLBS requestBody'
                  , requestHeaders = [("Content-Type", "application/json")]
                  }
    response <- httpLbs request manager
    let acceptedBySES = decode (responseBody response) :: Maybe EmailSent 
    print $ "acceptedBySES = " <> show acceptedBySES
    case acceptedBySES of 
        Nothing -> pure False 
        Just (EmailSent True) -> pure True 
        Just (EmailSent False) -> pure False   

