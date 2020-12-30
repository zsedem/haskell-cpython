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

module CPython.Types.InstanceMethod
  ( InstanceMethod
  , instanceMethodType
  , new
  , function
  ) where

#include <hscpython-shim.h>

import           CPython.Internal hiding (new)

newtype InstanceMethod = InstanceMethod (ForeignPtr InstanceMethod)

instance Object InstanceMethod where
  toObject (InstanceMethod x) = SomeObject x
  fromForeignPtr = InstanceMethod

instance Concrete InstanceMethod where
  concreteType _ = instanceMethodType

{# fun pure unsafe hscpython_PyInstanceMethod_Type as instanceMethodType
  {} -> `Type' peekStaticObject* #}

{# fun PyInstanceMethod_New as new
  `Object func' =>
  { withObject* `func'
  } -> `InstanceMethod' stealObject* #}

{# fun PyInstanceMethod_Function as function
  { withObject* `InstanceMethod'
  } -> `SomeObject' peekObject* #}
