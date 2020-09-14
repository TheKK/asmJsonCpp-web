{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Frontend where

import Common.Route
import Control.Monad.IO.Class
import Data.Foldable
import Data.Time
import Data.Traversable
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Network
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.scss" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ blank mainBody
  }

data CounterEvent = Done | Cancel | Counting Int
  deriving (Show)

isDone :: CounterEvent -> Bool
isDone Done = True
isDone _ = False

countDown :: _ => Event t a -> NominalDiffTime -> Int -> m (Event t Int)
countDown e interval c = do
  cur <- liftIO getCurrentTime
  ticksE <- tickLossy interval cur
  es <- zipListWithEvent (\a _ -> a) (take c $ iterate pred (c - 1)) ticksE

  return $ leftmost [c <$ e, es]

raceTest :: _ => m ()
raceTest = elClass "div" "raceTest" $ mdo
  -- State
  let initValue = 100

  eventDyn <- holdDyn (pure never) $ leftmost
    [ countDown startE 0.02 initValue <$ startE
    , (pure never) <$ (canceledE <> resetE)
    ]

  eE <- switchHold never =<< networkView eventDyn

  sDyn <- holdDyn 0 $ leftmost
    [ eE
    , 0 <$ resetE
    ]

  -- UI
  startE <- button "START"
  canceledE <- button "STOP"
  resetE <- button "RESET"

  display sDyn

  -- Exports
  return ()

fooBody :: _ => m ()
fooBody = elAttr "div" attrs $ mdo
  -- State
  checkedTrueE' <- debounce 1 checkedTrueE

  let
    checkedE = updated checkedDyn
    checkedTrueE = () <$ ffilter id checkedE
    checkedFalseE = () <$ ffilter not checkedE

  t <- holdDyn Cancel $ leftmost
    [ Cancel <$ checkedFalseE
    , Done <$ checkedTrueE'
    ]

  -- UI
  text "Reflex-FRP!"
  checkedDyn <- Frontend.checkbox
  _ <- mkButton "next" $ updated $ fmap isDone t

  -- Exports
  return ()

  where
    attrs = mkAttrs [
      "href" =: "https://reflex-frp.org",
      "style" =: styles
      ]

    styles = " " <> fold [
      "width: 600px;",
      "height: 200px;",
      "box-shadow: #00000087 1px 1px 7px;",
      "border-radius: 5px"
      ]

realBody :: _ => m ()
realBody = elAttr "div" attrs $ mdo
  -- Event
  let
    areaInputE = _textAreaElement_input inputArea

  debouncedAreaInputE <- debounce 0.7 areaInputE

  -- State
  inputStatusDyn <- foldDyn ($) "-" $ leftmost
    [ const "..." <$ areaInputE
    , const "Done"  <$ debouncedAreaInputE
    ]

  sDyn <- holdDyn "EMPTY" debouncedAreaInputE

  -- UI
  inputArea <- divClass "a" $ mdo
    inputArea <- divClass "input-area" $
      textAreaElement $ def
        & initialAttributes .~ fold []

    divClass "input-indicator" $
      dynText inputStatusDyn

    return inputArea

  divClass "output-area" $
    dynText sDyn

  -- Export
  return ()

  where
    attrs = fold [
      "class" =: "real-body",
      "style" =: styles
      ]

    styles = " " <> fold [
      "width: 600px;",
      "height: 200px;",
      "display: flex;"
      ]

css :: [Map.Map T.Text T.Text] -> T.Text
css = fold . fmap go . Map.toList . fold
  where
    go (k, v) = k <> ": " <> v <> ";"

selector :: _ => [(T.Text, m ())] -> m ()
selector pages = mdo
  -- State
  clicksEs <- for pages $ \(t, ma) -> do
    e <- button t
    return $ ma <$ e

  _ <- networkHold blank $ leftmost clicksEs

  return ()

mainBody :: _ => m ()
mainBody = elClass "div" "mainBody" $ mdo
  -- UI
  selector
    [ ("real", realBody)
    , ("foo", fooBody)
    , ("race", raceTest)
    ]

  -- Exports
  return ()

mkAttrs :: [Map.Map T.Text T.Text] -> Map.Map T.Text T.Text
mkAttrs = fold

checkbox :: _ => m (Dynamic t Bool)
checkbox = mdo
  e <- inputElement $ def
    & inputElementConfig_initialChecked .~ False
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("type" =: "checkbox")


  return $ _inputElement_checked e

mkButton :: (DomBuilder t m, _) => T.Text -> Event t Bool -> m (Event t ())
mkButton t enabledE = mdo
  e <- inputElement $ def
    & inputElementConfig_initialValue .~ t
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("type" =: "button" <>
       "disabled" =: "true")
    & modifyAttributes .~
      (Map.singleton "disabled" <$> (fmap enabled enabledE))

  return $ domEvent Click e

  where
    enabled False = Just "true"
    enabled True = Nothing
