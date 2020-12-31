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

module CPython.Protocols.Mapping
  ( Mapping (..)
  , SomeMapping
  , castToMapping
  , getItem
  , setItem
  , deleteItem
  , size
  , hasKey
  , keys
  , values
  , items
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

instance Mapping Dictionary where
  toMapping = unsafeCastToMapping

castToMapping :: Object a => a -> IO (Maybe SomeMapping)
castToMapping obj =
  withObject obj $ \objPtr -> do
  isMapping <- fmap cToBool $ {# call PyMapping_Check as ^ #} objPtr
  return $ if isMapping
    then Just $ unsafeCastToMapping obj
    else Nothing

{# fun PyObject_GetItem as getItem
  `(Mapping self, Object key)' =>
  { withObject* `self'
  , withObject* `key'
  } -> `SomeObject' stealObject* #}

{# fun PyObject_SetItem as setItem
  `(Mapping self, Object key, Object value)' =>
  { withObject* `self'
  , withObject* `key'
  , withObject* `value'
  } -> `()' checkStatusCode* #}

{# fun PyObject_DelItem as deleteItem
  `(Mapping self, Object key)' =>
  { withObject* `self'
  , withObject* `key'
  } -> `()' checkStatusCode* #}

{# fun PyMapping_Size as size
  `Mapping self' =>
  { withObject* `self'
  } -> `Integer' checkIntReturn* #}

{# fun PyMapping_HasKey as hasKey
  `(Mapping self, Object key)' =>
  { withObject* `self'
  , withObject* `key'
  } -> `Bool' #}

{# fun PyMapping_Keys as keys
  `Mapping self' =>
  { withObject* `self'
  } -> `List' stealObject* #}

{# fun PyMapping_Values as values
  `Mapping self' =>
  { withObject* `self'
  } -> `List' stealObject* #}

{# fun PyMapping_Items as items
  `Mapping self' =>
  { withObject* `self'
  } -> `List' stealObject* #}
