{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rename modules.

module Main where

import           Data.Char
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Options.Applicative.Simple
import           Text.Regex.Applicative.Text

--------------------------------------------------------------------------------
-- Main entry point

data Config =
  Config
    { configFrom :: Text
    , configTo :: Text
    , configModules :: [FilePath]
    , configDryRun :: !Bool
    } deriving (Show)

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      "0"
      "Haskell module rename"
      "This program renames modules."
      (Config
        <$> fmap T.pack (strOption (long "from" <> help "The original module name"))
        <*> fmap T.pack (strOption (long "to" <> help "The target module name"))
        <*> some (strArgument (metavar "MODULEPATH" <> help "Filepath for a module"))
        <*> flag False True (long "dry-run" <> help "Don't make changes, just print what would be done."))
      empty
  mapM_ (rename opts) (configModules opts)

--------------------------------------------------------------------------------
-- Commands

rename :: Config -> FilePath -> IO ()
rename config path = do
  contents <- T.readFile path
  if configDryRun config
    then T.putStrLn (applyToContents config contents)
    else T.writeFile path (applyToContents config contents)

--------------------------------------------------------------------------------
-- Regex replacements

applyToContents :: Config -> Text -> Text
applyToContents config = replace (inImport mname) . replace inHeader
  where
    inHeader = line <> string "module" <> spaces1 <> mname <> spaces1
    mname = string (configFrom config) *> pure (configTo config)

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
