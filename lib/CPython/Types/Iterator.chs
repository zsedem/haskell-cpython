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

module CPython.Types.Iterator
  ( SequenceIterator
  , sequenceIteratorType
  , sequenceIteratorNew
  
  , CallableIterator
  , callableIteratorType
  , callableIteratorNew
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

newtype SequenceIterator = SequenceIterator (ForeignPtr SequenceIterator)

instance Iterator SequenceIterator where
  toIterator = unsafeCastToIterator

instance Object SequenceIterator where
  toObject (SequenceIterator x) = SomeObject x
  fromForeignPtr = SequenceIterator

instance Concrete SequenceIterator where
  concreteType _ = sequenceIteratorType

newtype CallableIterator = CallableIterator (ForeignPtr CallableIterator)

instance Iterator CallableIterator where
  toIterator = unsafeCastToIterator

instance Object CallableIterator where
  toObject (CallableIterator x) = SomeObject x
  fromForeignPtr = CallableIterator

instance Concrete CallableIterator where
  concreteType _ = callableIteratorType

{# fun pure unsafe hscpython_PySeqIter_Type as sequenceIteratorType
  {} -> `Type' peekStaticObject* #}

{# fun pure unsafe hscpython_PyCallIter_Type as callableIteratorType
  {} -> `Type' peekStaticObject* #}

-- | Return an 'Iterator' that works with a general sequence object, /seq/.
-- The iteration ends when the sequence raises @IndexError@ for the
-- subscripting operation.
{# fun PySeqIter_New as sequenceIteratorNew
  `Sequence seq' =>
  { withObject* `seq'
  } -> `SequenceIterator' stealObject* #}

-- | Return a new 'Iterator'. The first parameter, /callable/, can be any
-- Python callable object that can be called with no parameters; each call
-- to it should return the next item in the iteration. When /callable/
-- returns a value equal to /sentinel/, the iteration will be terminated.
{# fun PyCallIter_New as callableIteratorNew
  `(Object callable, Object sentinel)' =>
  { withObject* `callable'
  , withObject* `sentinel'
  } -> `CallableIterator' stealObject* #}
