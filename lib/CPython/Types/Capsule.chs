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

module CPython.Types.Capsule
	( Capsule
	, capsuleType
	, new
	, getPointer
	--, getDestructor
	, getContext
	, getName
	, importNamed
	, isValid
	, setPointer
	--, setDestructor
	, setContext
	--, setName
	) where

#include <hscpython-shim.h>

import           Data.Text (Text)

import           CPython.Internal hiding (new)
import           Foreign.Concurrent

type Destructor = Ptr () -> IO ()
newtype Capsule = Capsule (ForeignPtr Capsule)

instance Object Capsule where
	toObject (Capsule x) = SomeObject x
	fromForeignPtr = Capsule

instance Concrete Capsule where
	concreteType _ = capsuleType

{# fun pure unsafe hscpython_PyCapsule_Type as capsuleType
	{} -> `Type' peekStaticObject* #}

new :: Ptr () -> Maybe Text -> Destructor -> IO Capsule
new pointer name destructor = maybeWith withText name $ \namePtr -> do
    desturctorPtr <- mkDestructor destructor
    capsulePtr <- {# call PyCapsule_New as ^ #} pointer namePtr desturctorPtr
    stealObject capsulePtr

foreign import ccall "wrapper"
  mkDestructor :: Destructor -> IO (FunPtr Destructor)

-- | Retrieve the pointer stored in the capsule. On failure, throws an
-- exception.
--
-- The name parameter must compare exactly to the name stored in the capsule.
-- If the name stored in the capsule is 'Nothing', the name passed in must
-- also be 'Nothing'. Python uses the C function strcmp() to compare capsule
-- names.
getPointer :: Capsule -> Maybe Text -> IO (Ptr ())
getPointer py name =
	withObject py $ \pyPtr ->
	maybeWith withText name $ \namePtr ->
	{# call PyCapsule_GetPointer as ^ #} pyPtr namePtr

-- getDestructor :: Capsule -> IO (Maybe Destructor)
-- getDestructor = undefined

-- | Return the current context stored in the capsule, which might be @NULL@.
getContext :: Capsule -> IO (Ptr ())
getContext py =
	withObject py $ \pyPtr -> do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_GetContext as ^ #} pyPtr
	if ptr /= nullPtr
		then return ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return ptr

-- | Return the current name stored in the capsule, which might be 'Nothing'.
getName :: Capsule -> IO (Maybe Text)
getName py =
	withObject py $ \pyPtr -> do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_GetName as ^ #} pyPtr
	if ptr /= nullPtr
		then Just `fmap` peekText ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return Nothing

-- | Import a pointer to a C object from a capsule attribute in a module.
-- The name parameter should specify the full name to the attribute, as in
-- @\"module.attribute\"@. The name stored in the capsule must match this
-- string exactly. If the second parameter is 'False', import the module
-- without blocking (using @PyImport_ImportModuleNoBlock()@). Otherwise,
-- imports the module conventionally (using @PyImport_ImportModule()@).
--
-- Return the capsule&#x2019;s internal pointer on success. On failure, throw
-- an exception. If the module could not be imported, and if importing in
-- non-blocking mode, returns 'Nothing'.
importNamed :: Text -> Bool -> IO (Maybe (Ptr ()))
importNamed name block =
	withText name $ \namePtr ->
	let noBlock = cFromBool (not block) in do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_Import as ^ #} namePtr noBlock
	if ptr /= nullPtr
		then return $ Just ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return Nothing

-- | Determines whether or not a capsule is valid. A valid capsule's type is
-- 'capsuleType', has a non-NULL pointer stored in it, and its internal name
-- matches the name parameter. (See 'getPointer' for information on how
-- capsule names are compared.)
--
-- In other words, if 'isValid' returns 'True', calls to any of the
-- accessors (any function starting with @get@) are guaranteed to succeed.
isValid :: Capsule -> Maybe Text -> IO Bool
isValid py name =
	withObject py $ \pyPtr ->
	maybeWith withText name $ \namePtr ->
	{# call PyCapsule_IsValid as ^ #} pyPtr namePtr
	>>= checkBoolReturn

-- | Set the void pointer inside the capsule. The pointer may not be @NULL@.
{# fun PyCapsule_SetPointer as setPointer
	{ withObject* `Capsule'
	, id `Ptr ()'
	} -> `()' checkStatusCode* #}

-- setDestructor :: Capsule -> Maybe Destructor -> IO ()
-- setDestructor = undefined

-- | Set the context pointer inside the capsule.
{# fun PyCapsule_SetContext as setContext
	{ withObject* `Capsule'
	, id `Ptr ()'
	} -> `()' checkStatusCode* #}

-- setName :: Capsule -> Maybe Text -> IO ()
-- setName = undefined
