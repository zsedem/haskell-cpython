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

module CPython.Types.Type
  ( Type
  , typeType
  , isSubtype
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

instance Concrete Type where
  concreteType _ = typeType

{# fun pure unsafe hscpython_PyType_Type as typeType
  {} -> `Type' peekStaticObject* #}

-- | Returns 'True' if the first parameter is a subtype of the second
-- parameter.
{# fun PyType_IsSubtype as isSubtype
  { withObject* `Type'
  , withObject* `Type'
  } -> `Bool' #}
