{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

-- | Renaming modules.

module Hsmv where

import           Data.Char
import           Data.Maybe
#if !MIN_VERSION_base(4,9,0)
import           Data.Semigroup ((<>))
#endif
import           Data.Bool (bool)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Regex.Applicative.Text


--------------------------------------------------------------------------------
-- * Types

type ModuleName = Text
type DryRunFlag = Bool
type AutoQualifyFlag = Bool
type FileContents = Text
type ImportSrcLine = Text

data Import =
  Import
    { preamble :: Text
    , qualified :: Text
    , package :: Text
    , name :: ModuleName
    , as_ :: Text
    }


--------------------------------------------------------------------------------
-- * Regex helpers

capture :: RE' a -> RE Char Text
capture = fmap snd . withMatched

-- | Capture all space symbols
spaces1 :: RE Char Text
spaces1 = capture $ some $ psym isSpace

line :: RE Char Text
line = capture $ many $ psym (`elem` ['\n', '\r'])

mkRegexToImport :: RE Char Text -> RE Char Import
mkRegexToImport regexModuleName =
  Import
    <$> preamble
    <*> qualified
    <*> pkgImport
    <*> regexModuleName
    <*> as_
 where
   preamble = line <> string "import" <> spaces1
   qualified = captureRegex $ string "qualified"
   pkgImport = captureRegex $ string "\"" *> few (psym (/= '"')) *> string "\""
   as_ = captureRegex $ string "as"
   captureRegex re' = fmap (fromMaybe "") $ optional $ capture (re' *> spaces1)


--------------------------------------------------------------------------------
-- * Import transformations

inImport :: AutoQualifyFlag -> ModuleName -> RE Char Text -> RE Char Import
inImport qualifyFlag fromName regexModuleName =
  bool id (fmap $ qualify fromName) qualifyFlag $ mkRegexToImport regexModuleName

renderImport :: Import -> Text
renderImport Import {preamble, qualified, package, name, as_} =
  preamble <> qualified <> package <> name <> as_

qualify :: ModuleName -> Import -> Import
qualify fromName i@Import{as_ = "", name = name'} =
  i { name = fromMaybe name' mstripped
    , as_ = " as " <> fromName <> maybe "" (const "\n") mstripped
    }
 where mstripped = T.stripSuffix "\n" name'
qualify _ i = i

applyToContents :: AutoQualifyFlag -> ModuleName -> ModuleName -> FileContents -> FileContents
applyToContents qualifyFlag fromName toName =
  replace (fmap renderImport (inImport qualifyFlag fromName mname)) . replace inHeader
  where
    inHeader = line <> string "module" <> spaces1 <> mname
    mname = string fromName *> (pure toName <> spaces1)


#if !MIN_VERSION_regex_applicative(0,3,4)
--------------------------------------------------------------------------------
-- Orphan instances

instance Semigroup (RE' Text) where
  (<>) x y = (<>) <$> x <*> y
#endif
