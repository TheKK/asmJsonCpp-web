{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Frontend where

import Common.Route
import Data.Foldable
import Data.Maybe
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
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"normalize.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
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
    "* block comment as well!",
    "*/",
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

inputWidget :: _ => m (Dynamic t T.Text)
inputWidget = divClass "query-section" $ mdo
  -- State

  -- UI
  divClass "title" $ text "Query"

  inputArea' <- textAreaElement $ def
    & initialAttributes .~ fold
      [ "class" =: "input-area"
      ]
    & textAreaElementConfig_setValue .~ leftmost
      [ exampleSourceCode <$> (updated . _dropdown_value $ dropdown')
      ]

  dropdown' <- dropdown Example_Empty (constDyn $ fold
    [ Example_Empty =: "playground"
    , Example_ArrayOfString =: "array of string"
    , Example_AllFeatures =: "all features"
    ])
    def

  -- Exports
  return $ _textAreaElement_value inputArea'

resultWidget :: _ => Dynamic t T.Text -> m ()
resultWidget resultDyn = divClass "result-section" $ mdo
  -- State
  debouncedResultE <- debounce 0.7 $ updated resultDyn

  let
    testE = ffor debouncedResultE $ \query ->
      xhrRequest "GET" ("http://motherbrain.syno:5587/api/v1/compile?query=" <> encode query) def

  respE <- ffor (performRequestAsync testE) $
    fmap (fromMaybe "nothing" . _xhrResponse_responseText)

  respDyn <- holdDyn "^^^ TYPE SOMETHING TO PLAY ^^^" respE

  -- UI
  divClass "title" $ text "Result"
  divClass "result-area" $ dynText respDyn

  -- Exports
  return ()

  where
    -- XXX replace is not great.
    encode = T.replace "\n" "%0A" . T.replace " " "%20"

realBody :: _ => m ()
realBody = elAttr "div" attrs $ mdo
  -- State

  -- UI
  areaValueDyn <- inputWidget
  resultWidget areaValueDyn

  -- Export
  return ()

  where
    attrs = mkAttrs [
      "class" =: "real-body",
      "style" =: styles
      ]

    styles = css [
      "display" =: "flex"
      ]

css :: [Map.Map T.Text T.Text] -> T.Text
css = fold . fmap go . Map.toList . fold
  where
    go (k, v) = k <> ": " <> v <> ";"

selector :: _ => [(T.Text, m ())] -> m ()
selector pages = mdo
  -- State
  clicksEs <- divClass "selector-buttons" $
    for pages $ \(t, ma) -> do
      e <- button t
      return $ ma <$ e

  _ <- networkHold (fromMaybe blank $ snd <$> listToMaybe pages) $ leftmost clicksEs

  return ()

mainBody :: _ => m ()
mainBody = elClass "div" "mainBody" $ mdo
  -- UI
  selector
    [ ("real", realBody)
    ]

  -- Exports
  return ()

mkAttrs :: [Map.Map T.Text T.Text] -> Map.Map T.Text T.Text
mkAttrs = fold
