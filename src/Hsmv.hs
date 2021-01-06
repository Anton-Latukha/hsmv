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
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Regex.Applicative.Text

--------------------------------------------------------------------------------
-- Regex replacements

applyToContents :: Bool -> Text -> Text -> Text -> Text
applyToContents autoQualify from to =
  replace (fmap renderImport (inImport autoQualify from mname)) . replace inHeader
  where
    inHeader = line <> string "module" <> spaces1 <> mname
    mname = string from *> (pure to <> spaces1)

spaces1 :: RE Char Text
spaces1 = capture (some (psym isSpace))

line :: RE Char Text
line = capture (many (psym (\c -> c `elem` ['\n', '\r'])))

data Import =
  Import
    { preamble :: Text
    , qualified :: Text
    , package :: Text
    , name :: Text
    , as_ :: Text
    }

renderImport :: Import -> Text
renderImport Import {preamble, qualified, package, name, as_} =
  preamble <> qualified <> package <> name <> as_

inImport :: Bool -> Text -> RE Char Text -> RE Char Import
inImport autoQualify from mname =
  fmap
    qualify
    (Import <$> preamble <*> qualified <*> pkgImport <*> mname <*> as_)
  where
    qualify i =
      case i of
        Import {as_ = "", name = name'}
          | autoQualify -> i {as_ = " as " <> from <> maybe "" (const "\n") mstripped
                             ,name = fromMaybe name' mstripped}
          where mstripped = T.stripSuffix "\n" name'
        _ -> i
    preamble = line <> string "import" <> spaces1
    pkgImport =
      fmap
        (fromMaybe "")
        (optional
           (capture
              (string "\"" *> few (psym (/= '"')) *> string "\"" *> spaces1)))
    qualified =
      fmap (fromMaybe "") (optional (capture (string "qualified" *> spaces1)))
    as_ = fmap (fromMaybe "") (optional (capture (string "as" *> spaces1)))

capture :: RE' a -> RE Char Text
capture = fmap snd . withMatched

#if !MIN_VERSION_regex_applicative(0,3,4)
--------------------------------------------------------------------------------
-- Orphan instances

instance Semigroup (RE' Text) where
  (<>) x y = (<>) <$> x <*> y
#endif
