{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Common.Route
import Data.Foldable
import Data.Maybe
import Control.Monad
import Data.Traversable
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Network
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Language.Javascript.JSaddle.Types (JSM, MonadJSM)

import qualified Frontend.Configs as C

runOnEvent :: (Adjustable t m, MonadHold t m, MonadJSM m) => Event t a -> (a -> JSM b) -> m ()
runOnEvent e jsCode = void $ networkHold blank $ (ffor e $ \v -> liftJSM $ void $ jsCode v)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"normalize.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"bulma.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"line-awesome-1.3.0/css/line-awesome.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = prerender_ blank mainBody
  }

data Example
  = Example_Empty
  | Example_ArrayOfString
  | Example_AllFeatures
  deriving (Eq, Ord)

exampleSourceCode :: Example -> T.Text
exampleSourceCode Example_Empty = ""
exampleSourceCode Example_ArrayOfString = "AsArray EachElement AsString"
exampleSourceCode Example_AllFeatures = T.init . T.unlines $
  [
    "// Support line comment!",
    "",
    "/* Support",
    " * block comment as well!",
    " */",
    "",
    "// Parse fields of object into struct.",
    "AsObj FieldsToStruct MagicStructName [",
    "  // You can parse int.",
    "  (i, AsInt),",
    "",
    "  // Parse string.",
    "  (s, AsString),",
    "",
    "  // Parse into vector.",
    "  (vector_of_i, AsArray EachElement AsInt),",
    "",
    "  // Parse item inside array.",
    "  (one_of_array, AsArray AtNth 42 AsString),",
    "",
    "  // Parse field of object.",
    "  (in_obj, AsObj AtField xyz AsInt),",
    "",
    "  // Parse indexes into struct.",
    "  (indexes, AsArray IndexesToStruct Indexes [(1, first, AsString), (2, second, AsInt),])",
    "]"
  ]

inputWidget :: (MonadWidget t m) => m (Dynamic t T.Text)
inputWidget = mdo
  -- State

  -- UI
  divClass "title is-5" $ text "Query"

  inputArea' <- divClass "block" $ textAreaElement $ def
    & initialAttributes .~ fold
      [ "class" =: "textarea is-family-code"
      , "rows" =: "10"
      ]
    & textAreaElementConfig_setValue .~ leftmost
      [ exampleSourceCode <$> (updated . _dropdown_value $ dropdown')
      ]

  dropdown' <- divClass "select" $ dropdown Example_Empty (constDyn $ fold
    [ Example_Empty =: "playground"
    , Example_ArrayOfString =: "array of string"
    , Example_AllFeatures =: "all features"
    ])
    def

  -- Exports
  return $ _textAreaElement_value inputArea'

copyButton :: (MonadWidget t m, HasConfigs m) => T.Text -> m ()
copyButton query = mdo
  -- Events
  runOnEvent (domEvent Click e) $ \() -> do
    eval . fold $
      [ "document.querySelector('" <> query <> "').select();",
        "document.execCommand('copy');"
      ]

  -- UI
  (e, _) <-
    elClass' "button" "button is-primary" $ do
      elClass "span" "icon" $ elClass "i" "las la-clipboard la-lg" blank
      el "span" $ text "Copy"

  -- Exports
  return ()

compileV1 :: (MonadWidget t m, HasConfigs m) => Event t T.Text -> m (Event t T.Text)
compileV1 queryE = do
  apiUrl <- C.getAPIURL

  respE <- performRequestAsync
    $ fmap (\url -> xhrRequest "GET" url def)
    $ fmap (\query -> apiUrl <> "/api/v1/compile?query=" <> query)
    $ fmap encode
    $ queryE

  return $ fmap (fromMaybe "nothing" . _xhrResponse_responseText) respE

  where
    -- XXX replace is not great.
    encode = T.replace "\n" "%0A" . T.replace " " "%20"

resultWidget :: (MonadWidget t m, HasConfigs m) => Dynamic t T.Text -> m ()
resultWidget resultDyn = do
  -- State
  debouncedResultE <- debounce 0.7 $ updated resultDyn

  -- Events
  respE <- compileV1 debouncedResultE

  -- UI
  divClass "title is-5" $ text "Result"
  inputArea' <- divClass "block" $ textAreaElement $ def
    & initialAttributes .~ fold
      [ "class" =: "result-area textarea is-family-code"
      , "rows" =: "10"
      , "readonly" =: ""
      ]
    & textAreaElementConfig_initialValue .~ "^^^ TYPE SOMETHING TO PLAY ^^^"
    & textAreaElementConfig_setValue .~ respE
  copyButton ".result-area"

  -- Exports
  return ()

pageTitle :: (MonadWidget t m) => m ()
pageTitle = do
  divClass "title" $ text "AsmJsonCpp"
  divClass "subtitle" $ text "Do the repeating job for you, safely and more efficiently than you. Sorry, human beings."

heartIcon :: MonadWidget t m => m ()
heartIcon = elClass "i" "las la-heart" blank

haskellLink :: MonadWidget t m => m ()
haskellLink = elAttr "a" ("href" =: "https://www.haskell.org/" <> "target" =: "_blank")
  $ text "Haskell"

footer :: MonadWidget t m => m ()
footer =
  elAttr "div" attrs $ do
    text "Made with " >> heartIcon >> text " and " >> haskellLink >> text " by TheKK"

  where
    attrs = mkAttrs
      [ "style" =: css
          [ "text-align" =: "center"
          , "padding-bottom" =: "15px"
          ]
      ]

realBody :: (MonadWidget t m, HasConfigs m) => m ()
realBody = do
  -- State

  -- UI
  elAttr "div" attrs $ do
    pageTitle
    elClass "div" "columns is-desktop" $ do
      areaValueDyn <- divClass "column" $ inputWidget
      divClass "column" $ resultWidget areaValueDyn
  footer

  -- Export
  return ()

  where
    attrs = mkAttrs [
      "class" =: "real-body",
      "style" =: styles
      ]

    styles = css
      [ "display" =: "flex"
      , "flex" =: "1"
      ]

css :: [Map.Map T.Text T.Text] -> T.Text
css = fold . fmap go . Map.toList . fold
  where
    go (k, v) = k <> ": " <> v <> ";"

selector :: (MonadWidget t m) => [(T.Text, m ())] -> m ()
selector pages = mdo
  -- State
  clicksEs <- divClass "selector-buttons" $
    for pages $ \(t, ma) -> do
      e <- button t
      return $ ma <$ e

  _ <- widgetHold (fromMaybe blank $ snd <$> listToMaybe pages) $ leftmost clicksEs

  return ()

mainBody :: (MonadWidget t m, HasConfigs m) => m ()
mainBody = elAttr "div" attrs $ mdo
  -- UI
  realBody

  -- Exports
  return ()

  where
    attrs = mkAttrs
      [ "style" =: css
          [ "display" =: "flex"
          , "flex-direction" =: "column"
          , "height" =: "100vh"
          ]
      ]

mkAttrs :: [Map.Map T.Text T.Text] -> Map.Map T.Text T.Text
mkAttrs = fold
