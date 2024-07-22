{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--

module CPython.Types.Module
  ( Module
  , moduleType
  , new
  , getDictionary
  , getName
  , getFilename
  , addObject
  , addIntegerConstant
  , addTextConstant
  , importModule
  , reload
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (toInteger)
import           Control.Exception (catch, throwIO, SomeException)
import           Data.Semigroup ((<>))
import           Data.Text (Text, unpack)

import           CPython.Internal hiding (new)
import           CPython.Types.Integer (toInteger)
import           CPython.Types.Unicode (toUnicode)

newtype Module = Module (ForeignPtr Module)

instance Object Module where
  toObject (Module x) = SomeObject x
  fromForeignPtr = Module

instance Concrete Module where
  concreteType _ = moduleType

{# fun pure unsafe hscpython_PyModule_Type as moduleType
  {} -> `Type' peekStaticObject* #}

-- | Return a new module object with the @__name__@ attribute set. Only the
-- module&#x2019;s @__doc__@ and @__name__@ attributes are filled in; the
-- caller is responsible for providing a @__file__@ attribute.
{# fun PyModule_New as new
  { withText* `Text'
  } -> `Module' stealObject* #}

-- | Return the dictionary object that implements a module&#x2019;s namespace;
-- this object is the same as the @__dict__@ attribute of the module. This
-- computation never fails. It is recommended extensions use other
-- computations rather than directly manipulate a module&#x2019;s @__dict__@.
{# fun PyModule_GetDict as getDictionary
  { withObject* `Module'
  } -> `Dictionary' peekObject* #}

-- | Returns a module&#x2019;s @__name__@ value. If the module does not
-- provide one, or if it is not a string, throws @SystemError@.
getName :: Module -> IO Text
getName py =
  withObject py $ \py' -> do
  raw <- {# call PyModule_GetName as ^ #} py'
  exceptionIf $ raw == nullPtr
  peekText raw

-- | Returns the name of the file from which a module was loaded using the
-- module&#x2019;s @__file__@ attribute. If this is not defined, or if it is
-- not a string, throws @SystemError@.
getFilename :: Module -> IO Text
getFilename py =
  withObject py $ \py' -> do
  raw <- {# call PyModule_GetFilename as ^ #} py'
  exceptionIf $ raw == nullPtr
  peekText raw

-- | Add an object to a module with the given name. This is a convenience
-- computation which can be used from the module&#x2019;s initialization
-- computation.
addObject :: Object value => Module -> Text -> value -> IO ()
addObject py name val =
  withObject py $ \py' ->
  withText name $ \name' ->
  withObject val $ \val' ->
  incref val' >>
  {# call PyModule_AddObject as ^ #} py' name' val'
  >>= checkStatusCode

-- | Add an integer constant to a module. This convenience computation can be
-- used from the module&#x2019;s initialization computation.
addIntegerConstant :: Module -> Text -> Integer -> IO ()
addIntegerConstant m name value = toInteger value >>= addObject m name

-- | Add a string constant to a module. This convenience computation can be
-- used from the module&#x2019;s initialization computation.
addTextConstant :: Module -> Text -> Text -> IO ()
addTextConstant m name value = toUnicode value >>= addObject m name

-- | This is a higher-level interface that calls the current &#x201c;import
-- hook&#x201d; (with an explicit level of @0@, meaning absolute import). It
-- invokes the @__import__()@ computation from the @__builtins__@ of the
-- current globals. This means that the import is done using whatever import
-- hooks are installed in the current environment.
--
-- This computation always uses absolute imports.
importModule :: Text -> IO Module
importModule name = (do
  pyName <- toUnicode name
  withObject pyName $ \namePtr ->
    {# call PyImport_Import as ^ #} namePtr
    >>= stealObject
  ) `catch` \(e :: SomeException) -> do
    let moduleName = unpack name
    putStrLn $
      "Error: Couldn't import Python module named `" <> moduleName <> "`\n" <>
      "Maybe it's a typo, or `" <> moduleName <> "` isn't installed?\n" <>
      "Check with `python -m " <> moduleName <> "`"
    throwIO e

-- | Reload a module. If an error occurs, an exception is thrown and the old
-- module still exists.
{# fun PyImport_ReloadModule as reload
  { withObject* `Module'
  } -> `Module' stealObject* #}
