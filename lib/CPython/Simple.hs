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

data Arg = forall a. ToPy a => Arg a

instance ToPy Arg where
  toPy (Arg a) = toPy a

arg :: ToPy a => a -> Arg
arg = Arg

initialize :: IO ()
initialize = Py.initialize

importModule :: Text -> IO Py.Module
importModule module_ = Py.importModule module_

call
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ function name
  -> [Arg] -- ^ args
  -> [(Text, Arg)] -- ^ kwargs
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

getAttribute
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ attribute name
  -> IO a
getAttribute moduleName name = do
  module_ <- importModule moduleName
  attr <- Py.getAttribute module_ =<< Py.toUnicode name
  fromPy attr
