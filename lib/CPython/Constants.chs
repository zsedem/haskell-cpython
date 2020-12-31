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

module CPython.Constants
  ( none
  , true
  , false
  , isNone
  , isTrue
  , isFalse
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

-- | The Python @None@ object, denoting lack of value.
{# fun unsafe hscpython_Py_None as none
  {} -> `SomeObject' peekObject* #}

-- | The Python @True@ object.
{# fun unsafe hscpython_Py_True as true
  {} -> `SomeObject' peekObject* #}

-- | The Python @False@ object.
{# fun unsafe hscpython_Py_False as false
  {} -> `SomeObject' peekObject* #}

{# fun pure unsafe hscpython_Py_None as rawNone
  {} -> `Ptr ()' id #}

{# fun pure unsafe hscpython_Py_True as rawTrue
  {} -> `Ptr ()' id #}

{# fun pure unsafe hscpython_Py_False as rawFalse
  {} -> `Ptr ()' id #}

isNone :: SomeObject -> IO Bool
isNone obj = withObject obj $ \ptr -> return $ ptr == rawNone

isTrue :: SomeObject -> IO Bool
isTrue obj = withObject obj $ \ptr -> return $ ptr == rawTrue

isFalse :: SomeObject -> IO Bool
isFalse obj = withObject obj $ \ptr -> return $ ptr == rawFalse
