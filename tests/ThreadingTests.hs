{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2020 Adam Zsigmond <zsedem@gmail.com>
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
  ( main ) where

import qualified CPython as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.List as PyList
import qualified CPython.Types.Tuple as PyTuple
import qualified CPython.Types.Unicode as PyUnicode
import qualified CPython.Types.Integer as PyInt
import qualified CPython.Types.Exception as PyExc
import Data.Text()
import Control.Concurrent
import Control.Monad
import Control.Exception(handle, SomeException)

main :: IO ()
main = verboseExc  $ do
  Py.initialize
  Py.evalInitThreads
  Py.saveThread
  mvars <- repeatM 10 $ myForkIO $ handle pyExceptionHandler testingSomePythonEvaluation
  _ <- forM mvars takeMVar
  return ()
--  Py.finalize
  where

    myForkIO :: IO () -> IO (MVar ())
    myForkIO io = do
      mvar <- newEmptyMVar
      let verboseIO = do myThreadId >>= \tid -> putStrLn ("Inside the forked thread: " ++ show tid)
                         Py.restoreThread
                         io
      _ <- forkFinally verboseIO (\_ -> do myThreadId >>= \tid -> putStrLn ("forkFinally end: " ++ show tid)
                                           Py.saveThread
                                           putMVar mvar ())
      return mvar
    pyExceptionHandler :: PyExc.Exception -> IO ()
    pyExceptionHandler exception = handle pyExceptionHandlerWithoutPythonTraceback $ do
        tracebackModule <- Py.importModule "traceback"
        print_exc <- PyUnicode.toUnicode "print_exception" >>= Py.getAttribute tracebackModule
        kwargs <- PyDict.new
        args <- case PyExc.exceptionTraceback exception of
          Just tb -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception, tb]
          _ -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception]
        _ <- Py.call print_exc args kwargs
        return ()
    pyExceptionHandlerWithoutPythonTraceback :: PyExc.Exception -> IO ()
    pyExceptionHandlerWithoutPythonTraceback exception = do
        print exception
        putStrLn "Unexpected Python exception (Please report a bug)"

    verboseExc ioAction = handleEverything (\exc -> print exc >> error "Unexpected error") ioAction

    handleEverything :: (SomeException -> IO a) -> IO a -> IO a
    handleEverything = handle


testingSomePythonEvaluation :: IO ()
testingSomePythonEvaluation = do
    let l :: [Integer]
        l = [1..10000]
    testList <- traverse toObj l >>= PyList.toList >>= (return . Py.toObject)
    builtinsModule <- Py.importModule "builtins"
    myThreadId >>= \tid -> putStrLn ("builtins module acquired: " ++ show tid)
    sumFunc <- PyUnicode.toUnicode "sum" >>= Py.getAttribute builtinsModule
    args <- PyTuple.toTuple [testList]
    kwargs <- PyDict.new
    myThreadId >>= \tid -> putStrLn ("Running the sum: " ++ show tid)
    _ <- Py.call sumFunc args kwargs
    myThreadId >>= \tid -> putStrLn ("End of python calculation: " ++ show tid)
    return ()
  where
    toObj integer = fmap Py.toObject $ PyInt.toInteger integer

repeatM :: Int -> IO a -> IO [a]
repeatM i action = forM [1..i] $ \_ -> action
