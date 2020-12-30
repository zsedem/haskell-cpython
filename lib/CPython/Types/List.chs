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

module CPython.Types.List
  ( List
  , listType
  , toList
  , iterableToList
  , fromList
  , length
  , getItem
  , setItem
  , insert
  , append
  , getSlice
  , setSlice
  , sort
  , reverse
  , toTuple
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (reverse, length)

import           CPython.Internal hiding (new)
import qualified CPython.Types.Tuple as T

instance Concrete List where
  concreteType _ = listType

{# fun pure unsafe hscpython_PyList_Type as listType
  {} -> `Type' peekStaticObject* #}

toList :: [SomeObject] -> IO List
toList xs =
  mapWith withObject xs $ \ptrs ->
  withArrayLen ptrs $ \count array ->
  {# call hscpython_poke_list #} (fromIntegral count) array
  >>= stealObject

-- | Convert any object implementing the iterator protocol to a 'List'.
iterableToList :: Object iter => iter -> IO List
iterableToList iter = do
  raw <- callObjectRaw listType =<< T.toTuple [toObject iter]
  return $ unsafeCast raw

fromList :: List -> IO [SomeObject]
fromList py =
  withObject py $ \pyPtr ->
  ({# call PyList_Size as ^ #} pyPtr >>=) $ \size ->
  let size' = fromIntegral size :: Int in
  withArray (replicate size' nullPtr) $ \ptrs ->
  {# call hscpython_peek_list #} pyPtr size ptrs >>
  peekArray size' ptrs >>= mapM peekObject

{# fun PyList_Size as length
  { withObject* `List'
  } -> `Integer' checkIntReturn* #}

-- | Returns the object at a given position in the list. The position must be
-- positive; indexing from the end of the list is not supported. If the
-- position is out of bounds, throws an @IndexError@ exception.
{# fun PyList_GetItem as getItem
  { withObject* `List'
  , fromIntegral `Integer'
  } -> `SomeObject' peekObject* #}

-- | Set the item at a given index.
setItem :: Object o => List -> Integer -> o -> IO ()
setItem self index x =
  withObject self $ \selfPtr ->
  withObject x $ \xPtr -> do
  incref xPtr
  {# call PyList_SetItem as ^ #} selfPtr (fromIntegral index) xPtr
  >>= checkStatusCode

-- | Inserts /item/ into the list in front of the given index. Throws an
-- exception if unsuccessful. Analogous to @list.insert(index, item)@.
{# fun PyList_Insert as insert
  `Object item' =>
  { withObject* `List'
  , fromIntegral `Integer'
  , withObject* `item'
  } -> `()' checkStatusCode* #}

-- | Append /item/ to the end of th list. Throws an exception if unsuccessful.
-- Analogous to @list.append(item)@.
{# fun PyList_Append as append
  `Object item' =>
  { withObject* `List'
  , withObject* `item'
  } -> `()' checkStatusCode* #}

-- | Return a list of the objects in list containing the objects between
-- the given indexes. Throws an exception if unsuccessful. Analogous to
-- @list[low:high]@. Negative indices, as when slicing from Python, are not
-- supported.
{# fun PyList_GetSlice as getSlice
  { withObject* `List'
  , fromIntegral `Integer'
  , fromIntegral `Integer'
  } -> `List' stealObject* #}

-- | Sets the slice of a list between /low/ and /high/ to the contents of
-- a replacement list. Analogous to @list[low:high] = replacement@. The
-- replacement may be 'Nothing', indicating the assignment of an empty list
-- (slice deletion). Negative indices, as when slicing from Python, are not
-- supported.
setSlice
  :: List
  -> Integer -- ^ Low
  -> Integer -- ^ High
  -> Maybe List -- ^ Replacement
  -> IO ()
setSlice self low high items = let
  low' = fromIntegral low
  high' = fromIntegral high in
  withObject self $ \selfPtr ->
  maybeWith withObject items $ \itemsPtr -> do
  {# call PyList_SetSlice as ^ #} selfPtr low' high' itemsPtr
  >>= checkStatusCode

-- | Sort the items of a list in place. This is equivalent to @list.sort()@.
{# fun PyList_Sort as sort
  { withObject* `List'
  } -> `()' checkStatusCode* #}

-- | Reverses the items of a list in place. This is equivalent to
-- @list.reverse()@.
{# fun PyList_Reverse as reverse
  { withObject* `List'
  } -> `()' checkStatusCode* #}

-- | Return a new 'Tuple' containing the contents of a list; equivalent to
-- @tuple(list)@.
{# fun PyList_AsTuple as toTuple
  { withObject* `List'
  } -> `Tuple' stealObject* #}
