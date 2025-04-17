{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE TemplateHaskell #-}

module JsonTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- import Data.Aeson.TH -- this module helps you customise json derivation
import Data.Time (UTCTime)
import Servant.Auth.JWT (FromJWT, ToJWT)

data Credentials = Credentials
    { email :: Text
    , password :: Text
    }
    deriving (Generic, Show, FromJSON, ToJSON)

data SignupResponse = SignupSuccess deriving (Generic, Show, FromJSON, ToJSON)

data SignUpErrors
    = AccountAlreadyExists
    | VerificationPending
    | EmailUnreachable
    deriving (Generic, Show, FromJSON, ToJSON)

data Session = Session
    { userEmail :: Text
    , expiresAt :: UTCTime
    , createdAt :: UTCTime
    }
    deriving (Generic, Show, FromJSON, ToJSON, ToJWT, FromJWT)

data Profile = Profile
    { userName :: Text
    , profilePic :: Text
    }
    deriving (Generic, Show, FromJSON, ToJSON)

newtype HomePageResponse = HomePageResponse
    { profile :: Maybe Profile
    }
    deriving (Generic, Show, FromJSON, ToJSON)

data CookieAuthErrors
    = NoCookieInHeader
    | NoSessionCookie
    | InvalidJWT
    | CookieExpired
    deriving (Generic, Show, FromJSON, ToJSON)

data LoginErrors
    = NoSuchEmail
    | PasswordMismatch
    deriving (Generic, Show, FromJSON, ToJSON)

newtype LoginResponse = LoggedIn {userProfile :: Profile}
    deriving (Generic, Show, FromJSON, ToJSON)
