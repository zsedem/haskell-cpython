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

module CPython.Types.Code
  ( Code
  , codeType
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

newtype Code = Code (ForeignPtr Code)

instance Object Code where
  toObject (Code x) = SomeObject x
  fromForeignPtr = Code

instance Concrete Code where
  concreteType _ = codeType

{# fun pure unsafe hscpython_PyCode_Type as codeType
  {} -> `Type' peekStaticObject* #}
