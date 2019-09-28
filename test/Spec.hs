{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import qualified Data.Text.IO as T
import           Hsmv
import           Test.Hspec

main = hspec spec

spec =
  describe
    "Sanity check"
    (do it
          "Replacements in file"
          (do out <- T.readFile "test/SanityOut.hs"
              shouldReturn
                (do contents <- T.readFile "test/Sanity.hs"
                    pure (applyToContents False "Foo" "Bar" contents))
                out)
        it
          "Replacements in file with auto-qualify"
          (do out <- T.readFile "test/SanityOutAutoQualify.hs"
              shouldReturn
                (do contents <- T.readFile "test/Sanity.hs"
                    pure (applyToContents True "Foo" "Bar" contents))
                out))
