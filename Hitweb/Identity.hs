{-# LANGUAGE TemplateHaskell #-}
module Hitweb.Identity where

import Prelude
import Database.Persist.TH

data IdValidationStatus
    = EmailNotAuthentified -- ^ A request for user creation has been sent
    | UserNotCreated       -- ^ Email has been authentified but no user entry created yet
    | UserCreated          -- ^ Email validated && user created
    deriving (Eq, Ord, Show, Read)

derivePersistField "IdValidationStatus"
