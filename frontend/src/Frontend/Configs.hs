{-# LANGUAGE OverloadedStrings #-}

module Frontend.Configs where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Configs

getAPIURL :: (HasConfigs m) => m T.Text
getAPIURL = T.strip . T.decodeUtf8 . fromMaybe "" <$> getConfig "frontend/api_url"
