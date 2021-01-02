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

module CPython.Types.Float
  ( Float
  , floatType
  , toFloat
  , fromFloat
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (Float)

import           CPython.Internal

newtype Float = Float (ForeignPtr Float)

instance Object Float where
  toObject (Float x) = SomeObject x
  fromForeignPtr = Float

instance Concrete Float where
  concreteType _ = floatType

{# fun pure unsafe hscpython_PyFloat_Type as floatType
  {} -> `Type' peekStaticObject* #}

{# fun PyFloat_FromDouble as toFloat
  { realToFrac `Double'
  } -> `Float' stealObject* #}

{# fun PyFloat_AsDouble as fromFloat
  { withObject* `Float'
  } -> `Double' realToFrac #}
