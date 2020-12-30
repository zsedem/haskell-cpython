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

module CPython.Types.WeakReference
  ( Reference
  , Proxy
  , newReference
  , newProxy
  , getObject
  ) where

#include <hscpython-shim.h>

import           CPython.Internal

newtype Reference = Reference (ForeignPtr Reference)
instance Object Reference where
  toObject (Reference x) = SomeObject x
  fromForeignPtr = Reference

newtype Proxy = Proxy (ForeignPtr Proxy)
instance Object Proxy where
  toObject (Proxy x) = SomeObject x
  fromForeignPtr = Proxy

-- | Return a weak reference for the object. This will always return a new
-- reference, but is not guaranteed to create a new object; an existing
-- reference object may be returned. The second parameter, /callback/, can
-- be a callable object that receives notification when /obj/ is garbage
-- collected; it should accept a single parameter, which will be the weak
-- reference object itself. If ob is not a weakly-referencable object, or if
-- /callback/ is not callable, this will throw a @TypeError@.
newReference :: (Object obj, Object callback) => obj -> Maybe callback -> IO Reference
newReference obj cb =
  withObject obj $ \objPtr ->
  maybeWith withObject cb $ \cbPtr ->
  {# call PyWeakref_NewRef as ^ #} objPtr cbPtr
  >>= stealObject

-- | Return a weak reference proxy for the object. This will always return a
-- new reference, but is not guaranteed to create a new object; an existing
-- proxy may be returned. The second parameter, /callback/, can be a callable
-- object that receives notification when /obj/ is garbage collected; it
-- should accept a single parameter, which will be the weak reference object
-- itself. If ob is not a weakly-referencable object, or if /callback/ is not
-- callable, this will throw a @TypeError@.
newProxy :: (Object obj, Object callback) => obj -> Maybe callback -> IO Proxy
newProxy obj cb =
  withObject obj $ \objPtr ->
  maybeWith withObject cb $ \cbPtr ->
  {# call PyWeakref_NewProxy as ^ #} objPtr cbPtr
  >>= stealObject

-- | Return the referenced object from a weak reference. If the referent is
-- no longer live, returns @None@.
{# fun PyWeakref_GetObject as getObject
  { withObject* `Reference'
  } -> `SomeObject' peekObject* #}
