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

module CPython.Types.ByteArray
  ( ByteArray
  , byteArrayType
  , toByteArray
  , fromByteArray
  , fromObject
  , append
  , length
  , resize
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (length)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           CPython.Internal

newtype ByteArray = ByteArray (ForeignPtr ByteArray)

instance Object ByteArray where
  toObject (ByteArray x) = SomeObject x
  fromForeignPtr = ByteArray

instance Concrete ByteArray where
  concreteType _ = byteArrayType

{# fun pure unsafe hscpython_PyByteArray_Type as byteArrayType
  {} -> `Type' peekStaticObject* #}

toByteArray :: B.ByteString -> IO ByteArray
toByteArray bytes = let
  mkByteArray = {# call PyByteArray_FromStringAndSize as ^ #}
  in B.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
     stealObject =<< mkByteArray cstr (fromIntegral len)

fromByteArray :: ByteArray -> IO B.ByteString
fromByteArray py =
  withObject py $ \pyPtr -> do
  size' <- {# call PyByteArray_Size as ^ #} pyPtr
  bytes <- {# call PyByteArray_AsString as ^ #} pyPtr
  B.packCStringLen (bytes, fromIntegral size')

-- | Create a new byte array from any object which implements the buffer
-- protocol.
{# fun PyByteArray_FromObject as fromObject
  `Object self ' =>
  { withObject* `self'
  } -> `ByteArray' stealObject* #}

{# fun PyByteArray_Concat as append
  { withObject* `ByteArray'
  , withObject* `ByteArray'
  } -> `ByteArray' stealObject* #}

{# fun PyByteArray_Size as length
  { withObject* `ByteArray'
  } -> `Integer' checkIntReturn* #}

{# fun PyByteArray_Resize as resize
  { withObject* `ByteArray'
  , fromIntegral `Integer'
  } -> `()' checkStatusCode* #}
