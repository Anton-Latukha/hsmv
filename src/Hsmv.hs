{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC  -fno-warn-orphans #-}

-- | Renaming modules.

module Hsmv where

import           Data.Char
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           Text.Regex.Applicative.Text

--------------------------------------------------------------------------------
-- Regex replacements

applyToContents :: Text -> Text -> Text -> Text
applyToContents from to = replace (inImport mname) . replace inHeader
  where
    inHeader = line <> string "module" <> spaces1 <> mname <> spaces1
    mname = string from *> pure to

spaces1 :: RE Char Text
spaces1 = capture (some (psym isSpace))

line :: RE Char Text
line = capture (many (psym (\c -> elem c ['\n', '\r'])))

inImport :: RE Char Text -> RE Char Text
inImport mname =
  line <> string "import" <> spaces1 <> qualified <> pkgImport <> mname
  where
    pkgImport =
      fmap
        (fromMaybe "")
        (optional
           (capture
              (string "\"" *> few (psym (/= '"')) *> string "\"" *> spaces1)))
    qualified =
      fmap (fromMaybe "") (optional (capture (string "qualified" *> spaces1)))

capture :: RE' a -> RE Char Text
capture = fmap snd . withMatched

--------------------------------------------------------------------------------
-- Orphan instances

instance Semigroup (RE' Text) where
  (<>) x y = (<>) <$> x <*> y
