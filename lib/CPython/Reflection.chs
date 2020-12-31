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
--
module CPython.Reflection
  ( getBuiltins
  , getLocals
  , getGlobals
  , getFrame
  , getFunctionName
  , getFunctionDescription
  ) where

#include <hscpython-shim.h>

import           Data.Text (Text)

import           CPython.Internal

-- | Return a 'Dictionary' of the builtins in the current execution frame,
-- or the interpreter of the thread state if no frame is currently executing.
{# fun PyEval_GetBuiltins as getBuiltins
  {} -> `Dictionary' peekObject* #}

-- | Return a 'Dictionary' of the local variables in the current execution
-- frame, or 'Nothing' if no frame is currently executing.
getLocals :: IO (Maybe Dictionary)
getLocals = {# call PyEval_GetLocals as ^#} >>= maybePeek peekObject

-- | Return a 'Dictionary' of the global variables in the current execution
-- frame, or 'Nothing' if no frame is currently executing.
getGlobals :: IO (Maybe Dictionary)
getGlobals = {# call PyEval_GetGlobals as ^#} >>= maybePeek peekObject

-- | Return the current thread state's frame, which is 'Nothing' if no frame
-- is currently executing.
getFrame :: IO (Maybe SomeObject)
getFrame = {# call PyEval_GetFrame as ^#} >>= maybePeek peekObject

-- | Return the name of /func/ if it is a function, class or instance object,
-- else the name of /func/'s type.
{# fun PyEval_GetFuncName as getFunctionName
  `Object func' =>
  { withObject* `func'
  } -> `Text' peekText* #}

-- | Return a description string, depending on the type of func. Return
-- values include @\"()\"@ for functions and methods, @\"constructor\"@,
-- @\"instance\"@, and @\"object\"@. Concatenated with the result of
-- 'getFunctionName', the result will be a description of /func/.
{# fun PyEval_GetFuncDesc as getFunctionDescription
  `Object func' =>
  { withObject* `func'
  } -> `Text' peekText* #}
