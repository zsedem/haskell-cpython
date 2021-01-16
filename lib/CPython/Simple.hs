{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPython.Simple
  ( arg
  , FromPy(..)
  , PyCastException(..)
  , ToPy(..)
  , call
  , easyFromPy
  , easyToPy
  , getAttribute
  , importModule
  , initialize
  , setAttribute
  )
where

import CPython.Simple.Instances

import Control.Exception (catch, SomeException)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import qualified CPython as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Tuple as Py (toTuple)
import qualified CPython.Types.Dictionary as PyDict

-- | Representation of an argument to a Python function
--
-- This lets us use various Haskell types in the same list of arguments. For example:
--
-- @
-- sampleArgs :: [Arg]
-- sampleArgs =
--   [ arg (7 :: Integer)
--   , arg ("hello" :: Text)
--   ]
-- @
data Arg = forall a. ToPy a => Arg a

instance ToPy Arg where
  toPy (Arg a) = toPy a

-- | Builds a Python argument from any Haskell type with a `ToPy` instance
arg :: ToPy a => a -> Arg
arg = Arg

-- | Kicks off talking to Python, and will need to be called before using other functions
initialize :: IO ()
initialize = Py.initialize

-- | Given a Python module name as text, imports it as a `Py.Module`
--
-- Throws an exception if e.g. the module name was misspelled, or isn't installed
importModule :: Text -> IO Py.Module
importModule module_ = Py.importModule module_

-- | The most common use case of `CPython.Simple` is calling some Python function
--
-- For example, if we wanted to wrap Python's @random.randint(low, high)@, we could write this:
--
-- @
-- randint :: Integer -> Integer -> IO Integer
-- randint low high =
--   call "random" "randint" [arg low, arg high] []
-- @
--
-- Because of the `FromPy` instance in `call`'s type signature, we can infer what to do to convert a Python value back into Haskell, if given the type. In this example using @random.uniform@, although we use a similar definition as for @randint@, we correct cast to `Double` instead of `Integer`
--
-- @
-- uniform :: Integer -> Integer -> IO Double
-- uniform low high =
--   call "random" "uniform" [arg low, arg high] []
-- @
--
-- We can also use the `TypeApplications` language extension to tell `call` what type to use, if needed
--
-- @
-- call @Double "random" "uniform" [arg low, arg high] []
-- @
--
-- Calling a function with mixed positional and keyword arguments is also fairly straightforward.
--
-- The example is equivalent to calling @pyautogui.moveTo(x, y, duration=seconds)@
--
-- @
-- moveToDuration :: Integer -> Integer -> Double -> IO ()
-- moveToDuration x y seconds =
--   call "pyautogui" "moveTo" [arg x, arg y] [("duration", arg seconds)]
-- @
call
  :: FromPy a
  => Text -- ^ Python module name
  -> Text -- ^ Python function name
  -> [Arg] -- ^ Python function's arguments
  -> [(Text, Arg)] -- ^ Python function's keyword arguments (kwargs) as @(name, value)@ pairs
  -> IO a
call moduleName func args kwargs = do
  module_ <- importModule moduleName
  pyFunc <- Py.getAttribute module_ =<< Py.toUnicode func
  pyArgs <- mapM toPy args
  pyArgsTuple <- Py.toTuple pyArgs
  pyKwargs <- toPyKwargs kwargs
  result <- Py.call pyFunc pyArgsTuple pyKwargs
  fromPy result
  where
    toPyKwargs :: [(Text, Arg)] -> IO Py.Dictionary
    toPyKwargs dict = do
      myDict <- PyDict.new
      mapM_
        (\(k, v) -> do
          pyKey <- toPy k
          pyVal <- toPy v
          PyDict.setItem myDict pyKey pyVal)
        dict
      pure myDict

-- | Set the value of an attribute of some Python module
--
-- This example is equivalent to setting @random.BPF = n@ in Python
--
-- @
-- setBpf :: Integer -> IO ()
-- setBpf n = setAttribute "random" \"BPF\" n
-- @
setAttribute
  :: ToPy a
  => Text -- ^ module name
  -> Text -- ^ attribute name
  -> a -- ^ value to set attribute to
  -> IO ()
setAttribute moduleName name value = do
  module_ <- importModule moduleName
  pyName <- Py.toUnicode name
  pyValue <- toPy value
  Py.setAttribute module_ pyName pyValue

-- | Get the value of an attribute of some Python module
--
-- This example is equivalent to getting @random.BPF@ in Python
--
-- @
-- getBpf :: IO Integer
-- getBpf = getAttribute "random" \"BPF\"
-- @
getAttribute
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ attribute name
  -> IO a
getAttribute moduleName name = do
  module_ <- importModule moduleName
  attr <- Py.getAttribute module_ =<< Py.toUnicode name
  fromPy attr
