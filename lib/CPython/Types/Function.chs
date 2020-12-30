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

module CPython.Types.Function
  ( Function
  , functionType
  , new
  , getCode
  , getGlobals
  , getModule
  , getDefaults
  , setDefaults
  , getClosure
  , setClosure
  , getAnnotations
  , setAnnotations
  ) where

#include <hscpython-shim.h>

import qualified CPython.Constants as Const
import           CPython.Internal hiding (new)
import           CPython.Types.Code (Code)

newtype Function = Function (ForeignPtr Function)

instance Object Function where
  toObject (Function x) = SomeObject x
  fromForeignPtr = Function

instance Concrete Function where
  concreteType _ = functionType

{# fun pure unsafe hscpython_PyFunction_Type as functionType
  {} -> `Type' peekStaticObject* #}

-- | Return a new function associated with the given code object. The second
-- parameter will be used as the globals accessible to the function.
--
-- The function's docstring, name, and @__module__@ are retrieved from the
-- code object. The parameter defaults and closure are set to 'Nothing'.
{# fun PyFunction_New as new
  { withObject* `Code'
  , withObject* `Dictionary'
  } -> `Function' stealObject* #}

-- | Return the code object associated with a function.
{# fun PyFunction_GetCode as getCode
  { withObject* `Function'
  } -> `Code' peekObject* #}

-- | Return the globals dictionary associated with a function.
{# fun PyFunction_GetGlobals as getGlobals
  { withObject* `Function'
  } -> `Dictionary' peekObject* #}

-- | Return the @__module__@ attribute of a function. This is normally
-- a 'Unicode' containing the module name, but can be set to any other
-- object by Python code.
{# fun PyFunction_GetModule as getModule
  { withObject* `Function'
  } -> `SomeObject' peekObject* #}

withNullableObject :: Object obj => Maybe obj -> (Ptr a -> IO b) -> IO b
withNullableObject Nothing io = do
  none <- Const.none
  withObject none io
withNullableObject (Just obj) io = withObject obj io

peekNullableObject :: Object obj => Ptr a -> IO (Maybe obj)
peekNullableObject = maybePeek peekObject

-- | Return the default parameter values for a function. This can be a tuple
-- or 'Nothing'.
{# fun PyFunction_GetDefaults as getDefaults
  { withObject* `Function'
  } -> `Maybe Tuple' peekNullableObject* #}

-- | Set the default values for a function.
{# fun PyFunction_SetDefaults as setDefaults
  { withObject* `Function'
  , withNullableObject* `Maybe Tuple'
  } -> `()' checkStatusCode* #}

-- | Return the closure associated with a function. This can be 'Nothing',
-- or a tuple of 'Cell's.
{# fun PyFunction_GetClosure as getClosure
  { withObject* `Function'
  } -> `Maybe Tuple' peekNullableObject* #}

-- | Set the closure associated with a function. The tuple should contain
-- 'Cell's.
{# fun PyFunction_SetClosure as setClosure
  { withObject* `Function'
  , withNullableObject* `Maybe Tuple'
  } -> `()' checkStatusCode* #}

-- | Return the annotations for a function. This can be a mutable dictionary,
-- or 'Nothing'.
{# fun PyFunction_GetAnnotations as getAnnotations
  { withObject* `Function'
  } -> `Maybe Dictionary' peekNullableObject* #}

-- | Set the annotations for a function object.
{# fun PyFunction_SetAnnotations as setAnnotations
  { withObject* `Function'
  , withNullableObject* `Maybe Dictionary'
  } -> `()' checkStatusCode* #}
