------------------------------------------------------------------------
-- |
-- Module      : DocTest.hs
-- Description : Http Client Auth Documentation Coverage
-- Copyright   : (c) 2015 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : unstable
-- Portability : POSIX

module Main (main) where


import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)


main :: IO ()
main = glob "src/**/[A-Z]*.hs" >>= doctest
