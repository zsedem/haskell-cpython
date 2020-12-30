{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main
  ( tests
  , main
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text

import           Test.Chell

import           CPython
import           CPython.Types.Module

suite_ProgramProperties :: Suite
suite_ProgramProperties = suite "program-properties"
  test_ProgramName
  test_PythonHome

test_ProgramName :: Test
test_ProgramName = assertions "program-name" $ do
  do
    name <- liftIO getProgramName
    $expect (Text.isPrefixOf "python" name)
  do
    before <- liftIO getProgramName
    liftIO (setProgramName "")
    after <- liftIO getProgramName
    $expect (equal before after)
  do
    liftIO (setProgramName "cpython-tests")
    after <- liftIO getProgramName
    $expect (equal after "cpython-tests")

test_PythonHome :: Test
test_PythonHome = assertions "python-home" $ do
  do
    defaultHome <- liftIO getPythonHome
    $expect (equal defaultHome Nothing)
  
  do
    liftIO (setPythonHome (Just "/python/home"))
    newHome <- liftIO getPythonHome
    $expect (equal newHome (Just "/python/home"))
  
  do
    liftIO (setPythonHome Nothing)
    newHome <- liftIO getPythonHome
    $expect (equal newHome Nothing)

suite_Types :: Suite
suite_Types = suite "types"
  suite_Module

suite_Module :: Suite
suite_Module = suite "module"
  test_ImportModule

test_ImportModule :: Test
test_ImportModule = assertions "importModule" $ do
  _ <- liftIO (importModule "os")
  return ()

tests :: [Suite]
tests =
  [ suite_ProgramProperties
  , suite_Types
  ]

main :: IO ()
main = do
  CPython.initialize
  Test.Chell.defaultMain tests
  CPython.finalize
