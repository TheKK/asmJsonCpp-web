{-# LANGUAGE OverloadedStrings #-}

module Frontend.Configs where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Configs

getServerURL :: (HasConfigs m) => m T.Text
getServerURL = T.decodeUtf8 . fromMaybe "" <$> getConfig "frontend/server_url"
