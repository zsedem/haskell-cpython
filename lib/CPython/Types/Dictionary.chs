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

module CPython.Types.Dictionary
  ( Dictionary
  , dictionaryType
  , new
  , clear
  , contains
  , copy
  , getItem
  , setItem
  , deleteItem
  , items
  , keys
  , values
  , size
  , merge
  , update
  , mergeFromSeq2
  ) where

#include <hscpython-shim.h>

import           CPython.Internal hiding (new)

instance Concrete Dictionary where
  concreteType _ = dictionaryType

{# fun pure unsafe hscpython_PyDict_Type as dictionaryType
  {} -> `Type' peekStaticObject* #}

{# fun PyDict_New as new
  {} -> `Dictionary' stealObject* #}

-- newProxy

-- | Empty an existing dictionary of all key-value pairs.
{# fun PyDict_Clear as clear
  { withObject* `Dictionary'
  } -> `()' id #}

-- | Determine if a dictionary contains /key/. If an item in the dictionary
-- matches /key/, return 'True', otherwise return 'False'. On error, throws
-- an exception. This is equivalent to the Python expression @key in d@.
{# fun PyDict_Contains as contains
  `Object key' =>
  { withObject* `Dictionary'
  , withObject* `key'
  } -> `Bool' checkBoolReturn* #}

-- | Return a new dictionary that contains the same key-value pairs as the
-- old dictionary.
{# fun PyDict_Copy as copy
  { withObject* `Dictionary'
  } -> `Dictionary' stealObject* #}

-- | Return the object from a dictionary which has a key /key/. Return
-- 'Nothing' if the key is not present.
getItem :: Object key => Dictionary -> key -> IO (Maybe SomeObject)
getItem dict key =
  withObject dict $ \dict' ->
  withObject key $ \key' -> do
  {# call PyErr_Clear as ^ #}
  raw <- {# call PyDict_GetItemWithError as ^ #} dict' key'
  if raw /= nullPtr
    then Just `fmap` peekObject raw
    else do
      exc <- {# call PyErr_Occurred as ^ #}
      exceptionIf $ exc /= nullPtr
      return Nothing

-- getItemString

-- | Inserts /value/ into a dictionary with a key of /key/. /key/ must be
-- hashable; if it isn&#x2019;t, throws @TypeError@.
{# fun PyDict_SetItem as setItem
  `(Object key, Object value)' =>
  { withObject* `Dictionary'
  , withObject* `key'
  , withObject* `value'
  } -> `()' checkStatusCode* #}

-- setItemString

-- | Remove the entry in a dictionary with key /key/. /key/ must be hashable;
-- if it isn&#x2019;t, throws @TypeError@.
{# fun PyDict_DelItem as deleteItem
  `Object key' =>
  { withObject* `Dictionary'
  , withObject* `key'
  } -> `()' checkStatusCode* #}

-- deleteItemString

-- | Return a 'List' containing all the items in the dictionary, as in
-- the Python method @dict.items()@.
{# fun PyDict_Items as items
  { withObject* `Dictionary'
  } -> `List' stealObject* #}

-- | Return a 'List' containing all the keys in the dictionary, as in
-- the Python method @dict.keys()@.
{# fun PyDict_Keys as keys
  { withObject* `Dictionary'
  } -> `List' stealObject* #}

-- | Return a 'List' containing all the values in the dictionary, as in
-- the Python method @dict.values()@.
{# fun PyDict_Values as values
  { withObject* `Dictionary'
  } -> `List' stealObject* #}

-- | Return the number of items in the dictionary. This is equivalent to
-- @len(d)@.
{# fun PyDict_Size as size
  { withObject* `Dictionary'
  } -> `Integer' checkIntReturn* #}

-- next

-- | Iterate over mapping object /b/ adding key-value pairs to a dictionary.
-- /b/ may be a dictionary, or any object supporting 'keys' and 'getItem'.
-- If the third parameter is 'True', existing pairs in will be replaced if a
-- matching key is found in /b/, otherwise pairs will only be added if there
-- is not already a matching key.
{# fun PyDict_Merge as merge
  `Mapping b' =>
  { withObject* `Dictionary'
  , withObject* `b'
  , `Bool'
  } -> `()' checkStatusCode* #}

-- | This is the same as @(\\a b -> 'merge' a b True)@ in Haskell, or
-- @a.update(b)@ in Python.
{# fun PyDict_Update as update
  `Mapping b' =>
  { withObject* `Dictionary'
  , withObject* `b'
  } -> `()' checkStatusCode* #}

-- | Update or merge into a dictionary, from the key-value pairs in /seq2/.
-- /seq2/ must be an iterable object producing iterable objects of length 2,
-- viewed as key-value pairs. In case of duplicate keys, the last wins if
-- the third parameter is 'True', otherwise the first wins. Equivalent
-- Python:
--
-- @
-- def mergeFromSeq2(a, seq2, override):
--   for key, value in seq2:
--     if override or key not in a:
--       a[key] = value
-- @
{# fun PyDict_MergeFromSeq2 as mergeFromSeq2
  `Object seq2' =>
  { withObject* `Dictionary'
  , withObject* `seq2'
  , `Bool'
  } -> `()' checkStatusCode* #}
