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

module CPython.Types.Integer
  ( Integer
  , integerType
  , toInteger
  , fromInteger
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (Integer, toInteger, fromInteger)
import qualified Prelude as Prelude
import qualified Data.Text as T

import           CPython.Internal
import qualified CPython.Protocols.Object as O
import qualified CPython.Types.Unicode as U

newtype Integer = Integer (ForeignPtr Integer)

instance Object Integer where
  toObject (Integer x) = SomeObject x
  fromForeignPtr = Integer

instance Concrete Integer where
  concreteType _ = integerType

{# fun pure unsafe hscpython_PyLong_Type as integerType
  {} -> `Type' peekStaticObject* #}

toInteger :: Prelude.Integer -> IO Integer
toInteger int = do
  let longlong = fromIntegral int
  let [_, min', max'] = [longlong, minBound, maxBound]
  stealObject =<< if Prelude.toInteger min' < int && int < Prelude.toInteger max'
    then {# call PyLong_FromLongLong as ^ #} longlong
    else withCString (show int) $ \cstr ->
      {# call PyLong_FromString as ^ #} cstr nullPtr 10

fromInteger :: Integer -> IO Prelude.Integer
fromInteger py = do
  (long, overflow) <- (withObject py $ \pyPtr ->
    alloca $ \overflowPtr -> do
    poke overflowPtr 0
    long <- {# call PyLong_AsLongAndOverflow as ^ #} pyPtr overflowPtr
    overflow <- peek overflowPtr
    return (long, overflow))
  if overflow == 0
    then return $ Prelude.toInteger long
    else fmap (read . T.unpack) $ U.fromUnicode =<< O.string py
