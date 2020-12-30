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

module CPython.Protocols.Sequence
  ( Sequence (..)
  , SomeSequence
  , castToSequence
  , length
  , append
  , repeat
  , inPlaceAppend
  , inPlaceRepeat
  , getItem
  , setItem
  , deleteItem
  , getSlice
  , setSlice
  , deleteSlice
  , count
  , contains
  , index
  , toList
  , toTuple
  , fast
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (repeat, length)
import           Data.Text (Text)

import           CPython.Internal
import           CPython.Types.ByteArray (ByteArray)
import           CPython.Types.Bytes (Bytes)
import           CPython.Types.Unicode (Unicode)

instance Sequence ByteArray where
  toSequence = unsafeCastToSequence

instance Sequence Bytes where
  toSequence = unsafeCastToSequence

instance Sequence List where
  toSequence = unsafeCastToSequence

instance Sequence Tuple where
  toSequence = unsafeCastToSequence

instance Sequence Unicode where
  toSequence = unsafeCastToSequence

-- | Attempt to convert an object to a generic 'Sequence'. If the object does
-- not implement the sequence protocol, returns 'Nothing'.
castToSequence :: Object a => a -> IO (Maybe SomeSequence)
castToSequence obj =
  withObject obj $ \objPtr -> do
  isSequence <- fmap cToBool $ {# call PySequence_Check as ^ #} objPtr
  return $ if isSequence
    then Just $ unsafeCastToSequence obj
    else Nothing

{# fun PySequence_Size as length
  `Sequence self' =>
  { withObject* `self'
  } -> `Integer' checkIntReturn* #}

{# fun PySequence_Concat as append
  `(Sequence a, Sequence b)' =>
  { withObject* `a'
  , withObject* `b'
  } -> `SomeSequence' stealObject* #}

{# fun PySequence_Repeat as repeat
  `Sequence a' =>
  { withObject* `a'
  , fromIntegral `Integer'
  } -> `a' stealObject* #}

{# fun PySequence_InPlaceConcat as inPlaceAppend
  `(Sequence a, Sequence b)' =>
  { withObject* `a'
  , withObject* `b'
  } -> `SomeSequence' stealObject* #}

{# fun PySequence_InPlaceRepeat as inPlaceRepeat
  `Sequence a' =>
  { withObject* `a'
  , fromIntegral `Integer'
  } -> `a' stealObject* #}

{# fun PySequence_GetItem as getItem
  `Sequence self' =>
  { withObject* `self'
  , fromIntegral `Integer'
  } -> `SomeObject' stealObject* #}

{# fun PySequence_SetItem as setItem
  `(Sequence self, Object v)' =>
  { withObject* `self'
  , fromIntegral `Integer'
  , withObject* `v'
  } -> `()' checkStatusCode* #}

{# fun PySequence_DelItem as deleteItem
  `Sequence self' =>
  { withObject* `self'
  , fromIntegral `Integer'
  } -> `()' checkStatusCode* #}

{# fun PySequence_GetSlice as getSlice
  `Sequence self' =>
  { withObject* `self'
  , fromIntegral `Integer'
  , fromIntegral `Integer'
  } -> `SomeObject' stealObject* #}

{# fun PySequence_SetSlice as setSlice
  `(Sequence self, Object v)' =>
  { withObject* `self'
  , fromIntegral `Integer'
  , fromIntegral `Integer'
  , withObject* `v'
  } -> `()' checkStatusCode* #}

{# fun PySequence_DelSlice as deleteSlice
  `Sequence self' =>
  { withObject* `self'
  , fromIntegral `Integer'
  , fromIntegral `Integer'
  } -> `()' checkStatusCode* #}

{# fun PySequence_Count as count
  `(Sequence self, Object v)' =>
  { withObject* `self'
  , withObject* `v'
  } -> `Integer' checkIntReturn* #}

{# fun PySequence_Contains as contains
  `(Sequence self, Object v)' =>
  { withObject* `self'
  , withObject* `v'
  } -> `Bool' checkBoolReturn* #}

-- | Return the first index /i/ for which @self[i] == v@. This is equivalent
-- to the Python expression @self.index(v)@.
{# fun PySequence_Index as index
  `(Sequence self, Object v)' =>
  { withObject* `self'
  , withObject* `v'
  } -> `Integer' checkIntReturn* #}

-- | Return a list object with the same contents as the arbitrary sequence
-- /seq/. The returned list is guaranteed to be new.
{# fun PySequence_List as toList
  `Sequence seq' =>
  { withObject* `seq'
  } -> `List' stealObject* #}

-- | Return a tuple object with the same contents as the arbitrary sequence
-- /seq/. If /seq/ is already a tuple, it is re-used rather than copied.
{# fun PySequence_Tuple as toTuple
  `Sequence seq' =>
  { withObject* `seq'
  } -> `Tuple' stealObject* #}

-- | Returns the sequence /seq/ as a tuple, unless it is already a tuple or
-- list, in which case /seq/ is returned. If an error occurs, throws
-- @TypeError@ with the given text as the exception text.
{# fun PySequence_Fast as fast
  `Sequence seq' =>
  { withObject* `seq'
  , withText* `Text'
  } -> `SomeSequence' stealObject* #}
