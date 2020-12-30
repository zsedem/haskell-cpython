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

module CPython.Types.Bytes
  ( Bytes
  , bytesType
  , toBytes
  , fromBytes
  , fromObject
  , length
  , append
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (length)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           CPython.Internal

newtype Bytes = Bytes (ForeignPtr Bytes)

instance Object Bytes where
  toObject (Bytes x) = SomeObject x
  fromForeignPtr = Bytes

instance Concrete Bytes where
  concreteType _ = bytesType

{# fun pure unsafe hscpython_PyBytes_Type as bytesType
  {} -> `Type' peekStaticObject* #}

toBytes :: B.ByteString -> IO Bytes
toBytes bytes = let
  mkBytes = {# call PyBytes_FromStringAndSize as ^ #}
  in B.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
     stealObject =<< mkBytes cstr (fromIntegral len)

fromBytes :: Bytes -> IO B.ByteString
fromBytes py =
  alloca $ \bytesPtr ->
  alloca $ \lenPtr ->
  withObject py $ \pyPtr -> do
  {# call PyBytes_AsStringAndSize as ^ #} pyPtr bytesPtr lenPtr
    >>= checkStatusCode
  bytes <- peek bytesPtr
  len <- peek lenPtr
  B.packCStringLen (bytes, fromIntegral len)

-- | Create a new byte string from any object which implements the buffer
-- protocol.
{# fun PyBytes_FromObject as fromObject
  `Object self ' =>
  { withObject* `self'
  } -> `Bytes' stealObject* #}

{# fun PyBytes_Size as length
  { withObject* `Bytes'
  } -> `Integer' checkIntReturn* #}

append :: Bytes -> Bytes -> IO Bytes
append self next =
  alloca $ \tempPtr ->
  withObject self $ \selfPtr -> do
  incref selfPtr
  poke tempPtr selfPtr
  withObject next $ \nextPtr -> do
  {# call PyBytes_Concat as ^ #} tempPtr nextPtr
  newSelf <- peek tempPtr
  stealObject newSelf
