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

module CPython.Types
  (
  -- * Types and classes
    ByteArray
  , Bytes
  , Capsule
  , Cell
  , Code
  , Complex
  , Dictionary
  , Exception
  , CPython.Types.Float.Float
  , Function
  , InstanceMethod
  , CPython.Types.Integer.Integer
  , SequenceIterator
  , CallableIterator
  , List
  , Method
  , Module
  , AnySet
  , Set
  , FrozenSet
  , Slice
  , Tuple
  , Type
  , Unicode
  , Reference
  , Proxy
  
  -- * Python 'Type' values
  , byteArrayType
  , bytesType
  , capsuleType
  , cellType
  , codeType
  , complexType
  , dictionaryType
  , floatType
  , functionType
  , instanceMethodType
  , integerType
  , sequenceIteratorType
  , callableIteratorType
  , listType
  , methodType
  , moduleType
  , setType
  , frozenSetType
  , sliceType
  , tupleType
  , typeType
  , unicodeType
  
  -- * Building and parsing values
  , toByteArray
  , fromByteArray
  , toBytes
  , fromBytes
  , toComplex
  , fromComplex
  , toFloat
  , fromFloat
  , CPython.Types.Integer.toInteger
  , CPython.Types.Integer.fromInteger
  , toList
  , iterableToList
  , fromList
  , toSet
  , toFrozenSet
  , iterableToSet
  , iterableToFrozenSet
  , fromSet
  , CPython.Types.Tuple.toTuple
  , iterableToTuple
  , fromTuple
  , toUnicode
  , fromUnicode
  ) where

import           CPython.Types.ByteArray
import           CPython.Types.Bytes
import           CPython.Types.Capsule
import           CPython.Types.Cell
import           CPython.Types.Code
import           CPython.Types.Complex
import           CPython.Types.Dictionary
import           CPython.Types.Exception
import           CPython.Types.Float
import           CPython.Types.Function
import           CPython.Types.InstanceMethod
import           CPython.Types.Integer
import           CPython.Types.Iterator
import           CPython.Types.List
import           CPython.Types.Method
import           CPython.Types.Module
import           CPython.Types.Set
import           CPython.Types.Slice
import           CPython.Types.Tuple
import           CPython.Types.Type
import           CPython.Types.Unicode
import           CPython.Types.WeakReference
