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

module CPython.Protocols.Iterator
  ( Iterator (..)
  , SomeIterator
  , castToIterator
  , next
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

-- | Attempt to convert an object to a generic 'Iterator'. If the object does
-- not implement the iterator protocol, returns 'Nothing'.
castToIterator :: Object a => a -> IO (Maybe SomeIterator)
castToIterator obj =
  withObject obj $ \objPtr -> do
  isIter <- fmap cToBool $ {# call hscpython_PyIter_Check as ^ #} objPtr
  return $ if isIter
    then Just $ unsafeCastToIterator obj
    else Nothing

-- | Return the next value from the iteration, or 'Nothing' if there are no
-- remaining items.
next :: Iterator iter => iter -> IO (Maybe SomeObject)
next iter =
  withObject iter $ \iterPtr -> do
  raw <- {# call PyIter_Next as ^ #} iterPtr
  if raw == nullPtr
    then do
      err <- {# call PyErr_Occurred as ^ #}
      exceptionIf $ err /= nullPtr
      return Nothing
    else fmap Just $ stealObject raw
