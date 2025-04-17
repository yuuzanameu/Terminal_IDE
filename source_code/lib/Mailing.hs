{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards  #-}

module Mailing (sendEmail ) where

import Network.Wreq
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client           (HttpException)
import Control.Exception             (try)
import GHC.Generics                  (Generic)
import Control.Lens                  ((&),(?~), (^.))
import Data.Text                     (Text)
import Data.Aeson                    (ToJSON, FromJSON)
import Data.ByteString               (ByteString)
import System.Environment            (getEnv)
import Data.ByteString.Char8         (pack)
import Text.Blaze.Html5              ((!), docTypeHtml, body, h1, p, a, toHtml, toValue)
import Text.Blaze.Html5.Attributes   (href,style)
import Text.Blaze.Html.Renderer.Text (renderHtml)


data EmailSettings = EmailSettings
    { from :: Text
    , to :: Text
    , subject :: Text
    , text :: Text
    }
    deriving (Generic, Show, ToJSON, FromJSON)

-- Mailgun API details
mailgunDomain :: String
mailgunDomain = "mail.editnow.site"

mailgunApiKey :: IO ByteString
mailgunApiKey = do
    key <- getEnv "MAILGUN_API_KEY"
    pure $ pack key


-- Mailgun API endpoint
mailgunUrl :: String
mailgunUrl = "https://api.eu.mailgun.net/v3/" <> mailgunDomain <> "/messages"


mkTemplate :: Text -> Text
mkTemplate link = L.toStrict $ renderHtml $ docTypeHtml $ do
    body $ do
        h1 "Welcome to Editnow"
        p "Please confirm sign-up by clicking the link below:"
        a ! href (toValue link) ! style "color: #007bff" $ "Confirm Sign-up"
        p "If the link above doesn't work, copy-paste this into the browser:"
        p (toHtml link)


defaultMailSettings :: EmailSettings
defaultMailSettings = EmailSettings
    { from="Editnow <verification@mail.editnow.site>"
    , to="Error "
    , subject="User email id verification mail"
    , text="Hi there!"
    }

type EmailId =  Text
type VerificationLink =  Text

sendEmail :: EmailId -> VerificationLink -> IO Bool
sendEmail email' = sendEmail' defaultMailSettings {to = email'}

sendEmail' :: EmailSettings -> Text -> IO Bool
sendEmail' EmailSettings{..} link = do
    key <- mailgunApiKey
    let opts = defaults & auth ?~ basicAuth "api" key
    let payload =
          [ partText "from" from
          , partText "to" to
          , partText "subject" subject
          , partText "text" text
          , partText "html" (mkTemplate link)
          ]
    result <- (try $ postWith opts mailgunUrl payload ) :: IO (Either HttpException (Response B.ByteString))
    case result of
        Left _ -> pure False
        Right response -> do
            let status = response ^. responseStatus . statusCode
            pure $ status == 200


