{-# LANGUAGE OverloadedStrings #-}

module Frontend.Debug (
  selector
) where

import qualified Data.Text as T
import Data.Traversable
import Data.Maybe
import Reflex.Dom.Core

selector :: MonadWidget t m => [(T.Text, m ())] -> m ()
selector pages = do
  -- State
  clicksEs <- divClass "selector-buttons" $
    for pages $ \(t, ma) -> do
      e <- button t
      return $ ma <$ e

  _ <- widgetHold (fromMaybe blank $ snd <$> listToMaybe pages) $ leftmost clicksEs

  return ()
