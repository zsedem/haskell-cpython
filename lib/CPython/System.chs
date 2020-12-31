{-# LANGUAGE ForeignFunctionInterface #-}

-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
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

module CPython.System
  ( getObject
  , setObject
  , deleteObject
  , resetWarnOptions
  , addWarnOption
  , setPath
  ) where

#include <hscpython-shim.h>

import           Data.Text (Text)

import           CPython.Internal

-- | Return the object /name/ from the @sys@ module, or 'Nothing' if it does
-- not exist.
getObject :: Text -> IO (Maybe SomeObject)
getObject name =
  withText name $ \cstr -> do
  raw <- {# call PySys_GetObject as ^ #} cstr
  maybePeek peekObject raw

-- getFile

-- | Set /name/ in the @sys@ module to a value.
setObject :: Object a => Text -> a -> IO ()
setObject name v =
  withText name $ \cstr ->
  withObject v $ \vPtr ->
  {# call PySys_SetObject as ^ #} cstr vPtr
  >>= checkStatusCode

-- | Delete /name/ from the @sys@ module.
deleteObject :: Text -> IO ()
deleteObject name =
  withText name $ \cstr ->
  {# call PySys_SetObject as ^ #} cstr nullPtr
  >>= checkStatusCode

-- | Reset @sys.warnoptions@ to an empty list.
{# fun PySys_ResetWarnOptions as resetWarnOptions
  {} -> `()' id #}

-- | Add an entry to @sys.warnoptions@.
addWarnOption :: Text -> IO ()
addWarnOption str = withTextW str pySysAddWarnOption

foreign import ccall safe "hscpython-shim.h PySys_AddWarnOption"
  pySysAddWarnOption :: CWString -> IO ()

-- | Set @sys.path@ to a list object of paths found in the parameter, which
-- should be a list of paths separated with the platform's search path
-- delimiter (@\':\'@ on Unix, @\';\'@ on Windows).
setPath :: Text -> IO ()
setPath path = withTextW path pySysSetPath

foreign import ccall safe "hscpython-shim.h PySys_SetPath"
  pySysSetPath :: CWString -> IO ()
