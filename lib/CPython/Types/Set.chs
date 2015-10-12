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

-- | Any functionality not listed below is best accessed using the either
-- the 'Object' protocol (including 'callMethod', 'richCompare', 'hash',
-- 'repr', 'isTrue', and 'getIter') or the 'Number' protocol (including 'and',
-- 'subtract', 'or', 'xor', 'inPlaceAnd', 'inPlaceSubtract', 'inPlaceOr',
-- and 'inPlaceXor').
module CPython.Types.Set
	( AnySet
	, Set
	, FrozenSet
	, setType
	, frozenSetType
	, toSet
	, toFrozenSet
	, iterableToSet
	, iterableToFrozenSet
	, fromSet
	, size
	, contains
	, add
	, discard
	, pop
	, clear
	) where

#include <hscpython-shim.h>

import           CPython.Internal
import           CPython.Types.Tuple (toTuple, iterableToTuple, fromTuple)

class Object a => AnySet a

newtype Set = Set (ForeignPtr Set)

instance Object Set where
	toObject (Set x) = SomeObject x
	fromForeignPtr = Set

instance Concrete Set where
	concreteType _ = setType

newtype FrozenSet = FrozenSet (ForeignPtr FrozenSet)

instance Object FrozenSet where
	toObject (FrozenSet x) = SomeObject x
	fromForeignPtr = FrozenSet

instance Concrete FrozenSet where
	concreteType _ = frozenSetType

instance AnySet Set
instance AnySet FrozenSet

{# fun pure unsafe hscpython_PySet_Type as setType
	{} -> `Type' peekStaticObject* #}

{# fun pure unsafe hscpython_PyFrozenSet_Type as frozenSetType
	{} -> `Type' peekStaticObject* #}

toSet :: [SomeObject] -> IO Set
toSet xs = toTuple xs >>= iterableToSet

toFrozenSet :: [SomeObject] -> IO FrozenSet
toFrozenSet xs = toTuple xs >>= iterableToFrozenSet

-- | Return a new 'Set' from the contents of an iterable 'Object'. The object
-- may be 'Nothing' to create an empty set. Throws a @TypeError@ if the object
-- is not iterable.
{# fun PySet_New as iterableToSet
	`Object obj' =>
	{ withObject* `obj'
	} -> `Set' stealObject* #}

-- | Return a new 'FrozenSet' from the contents of an iterable 'Object'. The
-- object may be 'Nothing' to create an empty frozen set. Throws a @TypeError@
-- if the object is not iterable.
{# fun PyFrozenSet_New as iterableToFrozenSet
	`Object obj' =>
	{ withObject* `obj'
	} -> `FrozenSet' stealObject* #}

fromSet :: AnySet set => set -> IO [SomeObject]
fromSet set = iterableToTuple set >>= fromTuple

-- | Return the size of a 'Set' or 'FrozenSet'.
{# fun PySet_Size as size
	`AnySet set' =>
	{ withObject* `set'
	} -> `Integer' checkIntReturn* #}

-- | Return 'True' if found, 'False' if not found. Unlike the Python
-- @__contains__()@ method, this computation does not automatically convert
-- unhashable 'Set's into temporary 'FrozenSet's. Throws a @TypeError@ if the
-- key is unhashable.
{# fun PySet_Contains as contains
	`(AnySet set, Object key)' =>
	{ withObject* `set'
	, withObject* `key'
	} -> `Bool' checkBoolReturn* #}

-- | Add /key/ to a 'Set'. Also works with 'FrozenSet' (like
-- 'CPython.Types.Tuple.setItem' it can be used to fill-in the values of
-- brand new 'FrozenSet's before they are exposed to other code). Throws a
-- @TypeError@ if the key is unhashable. Throws a @MemoryError@ if there is
-- no room to grow.
add :: (AnySet set, Object key) => set -> key -> IO ()
add = c_add

-- c2hs won't accept functions named "add" any more, so have it generate
-- c_add and then wrap that manually.
{# fun PySet_Add as c_add
	`(AnySet set, Object key)' =>
	{ withObject* `set'
	, withObject* `key'
	} -> `()' checkStatusCode* #}

-- | Return 'True' if found and removed, 'False' if not found (no action
-- taken). Does not throw @KeyError@ for missing keys. Throws a @TypeError@
-- if /key/ is unhashable. Unlike the Python @discard()@ method, this
-- computation does not automatically convert unhashable sets into temporary
-- 'FrozenSet's.
{# fun PySet_Discard as discard
	`Object key' =>
	{ withObject* `Set'
	, withObject* `key'
	} -> `Bool' checkBoolReturn* #}

-- | Return an arbitrary object in the set, and removes the object from the
-- set. Throws @KeyError@ if the set is empty.
{# fun PySet_Pop as pop
	{ withObject* `Set'
	} -> `SomeObject' stealObject* #}

-- | Remove all elements from a set.
{# fun PySet_Clear as clear
	{ withObject* `Set'
	} -> `()' checkStatusCode* #}
