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

module CPython.Protocols.Object
	( Object
	, Concrete
	, SomeObject
	
	-- * Types and casting
	, getType
	, isInstance
	, isSubclass
	, toObject
	, cast
	
	-- * Attributes
	, hasAttribute
	, getAttribute
	, setAttribute
	, deleteAttribute
	
	-- * Display and debugging
	, print
	, repr
	, ascii
	, string
	, bytes
	
	-- * Callables
	, callable
	, call
	, callArgs
	, callMethod
	, callMethodArgs
	
	-- * Misc
	, Comparison (..)
	, richCompare
	, toBool
	, hash
	, dir
	, getIterator
	) where

#include <hscpython-shim.h>

import           Prelude hiding (Ordering (..), print)
import qualified Data.Text as T
import           System.IO (Handle, hPutStrLn)

import           CPython.Internal hiding (toBool)
import           CPython.Protocols.Object.Enums
import qualified CPython.Types.Bytes as B
import qualified CPython.Types.Dictionary as D
import qualified CPython.Types.Tuple as Tuple
import qualified CPython.Types.Unicode as U

-- | Returns a 'Type' object corresponding to the object type of /self/. On
-- failure, throws @SystemError@. This is equivalent to the Python expression
-- @type(o)@.
{# fun PyObject_Type as getType
	`Object self' =>
	{ withObject* `self'
	} -> `Type' stealObject* #}

-- | Returns 'True' if /inst/ is an instance of the class /cls/ or a
-- subclass of /cls/, or 'False' if not. On error, throws an exception.
-- If /cls/ is a type object rather than a class object, 'isInstance'
-- returns 'True' if /inst/ is of type /cls/. If /cls/ is a tuple, the check
-- will be done against every entry in /cls/. The result will be 'True' when
-- at least one of the checks returns 'True', otherwise it will be 'False'. If
-- /inst/ is not a class instance and /cls/ is neither a type object, nor a
-- class object, nor a tuple, /inst/ must have a @__class__@ attribute &#2014;
-- the class relationship of the value of that attribute with /cls/ will be
-- used to determine the result of this function.
--
-- Subclass determination is done in a fairly straightforward way, but
-- includes a wrinkle that implementors of extensions to the class system
-- may want to be aware of. If A and B are class objects, B is a subclass of
-- A if it inherits from A either directly or indirectly. If either is not a
-- class object, a more general mechanism is used to determine the class
-- relationship of the two objects. When testing if B is a subclass of A, if
-- A is B, 'isSubclass' returns 'True'. If A and B are different objects,
-- B&#2018;s @__bases__@ attribute is searched in a depth-first fashion for
-- A &#2014; the presence of the @__bases__@ attribute is considered
-- sufficient for this determination.
{# fun PyObject_IsInstance as isInstance
	`(Object self, Object cls)' =>
	{ withObject* `self'
	, withObject* `cls'
	} -> `Bool' checkBoolReturn* #}

-- | Returns 'True' if the class /derived/ is identical to or derived from
-- the class /cls/, otherwise returns 'False'. In case of an error, throws
-- an exception. If /cls/ is a tuple, the check will be done against every
-- entry in /cls/. The result will be 'True' when at least one of the checks
-- returns 'True', otherwise it will be 'False'. If either /derived/ or /cls/
-- is not an actual class object (or tuple), this function uses the generic
-- algorithm described above.
{# fun PyObject_IsSubclass as isSubclass
	`(Object derived, Object cls)' =>
	{ withObject* `derived'
	, withObject* `cls'
	} -> `Bool' checkBoolReturn* #}

-- | Attempt to cast an object to some concrete class. If the object
-- isn't an instance of the class or subclass, returns 'Nothing'.
cast :: (Object a, Concrete b) => a -> IO (Maybe b)
cast obj = do
	let castObj = case toObject obj of
		SomeObject ptr -> fromForeignPtr $ castForeignPtr ptr
	validCast <- isInstance obj $ concreteType castObj
	return $ if validCast
		then Just castObj
		else Nothing

-- | Returns 'True' if /self/ has an attribute with the given name, and
-- 'False' otherwise. This is equivalent to the Python expression
-- @hasattr(self, name)@
{# fun PyObject_HasAttr as hasAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `Bool' checkBoolReturn* #}

-- | Retrieve an attribute with the given name from object /self/. Returns
-- the attribute value on success, and throws an exception on failure. This
-- is the equivalent of the Python expression @self.name@.
{# fun PyObject_GetAttr as getAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `SomeObject' stealObject* #}

-- | Set the value of the attribute with the given name, for object /self/,
-- to the value /v/. THrows an exception on failure. This is the equivalent
-- of the Python statement @self.name = v@.
{# fun PyObject_SetAttr as setAttribute
	`(Object self, Object v)' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	, withObject* `v'
	} -> `()' checkStatusCode* #}

-- | Delete an attribute with the given name, for object /self/. Throws an
-- exception on failure. This is the equivalent of the Python statement
-- @del self.name@.
{# fun hscpython_PyObject_DelAttr as deleteAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `()' checkStatusCode* #}

-- | Print @repr(self)@ to a handle.
print :: Object self => self -> Handle -> IO ()
print obj h = repr obj >>= U.fromUnicode >>= (hPutStrLn h . T.unpack)

-- | Compute a string representation of object /self/, or throw an exception
-- on failure. This is the equivalent of the Python expression @repr(self)@.
{# fun PyObject_Repr as repr
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

-- \ As 'ascii', compute a string representation of object /self/, but escape
-- the non-ASCII characters in the string returned by 'repr' with @\x@, @\u@
-- or @\U@ escapes. This generates a string similar to that returned by
-- 'repr' in Python 2.
{# fun PyObject_ASCII as ascii
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

-- | Compute a string representation of object /self/, or throw an exception
-- on failure. This is the equivalent of the Python expression @str(self)@.
{# fun PyObject_Str as string
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

-- | Compute a bytes representation of object /self/, or throw an exception
-- on failure. This is equivalent to the Python expression @bytes(self)@.
{# fun PyObject_Bytes as bytes
	`Object self' =>
	{ withObject* `self'
	} -> `B.Bytes' stealObject* #}

-- | Determine if the object /self/ is callable.
{# fun PyCallable_Check as callable
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

-- | Call a callable Python object /self/, with arguments given by the
-- tuple and named arguments given by the dictionary. Returns the result of
-- the call on success, or throws an exception on failure. This is the
-- equivalent of the Python expression @self(*args, **kw)@.
call :: Object self => self -> Tuple -> Dictionary -> IO SomeObject
call self args kwargs =
	withObject self $ \selfPtr ->
	withObject args $ \argsPtr ->
	withObject kwargs $ \kwargsPtr ->
	{# call PyObject_Call as ^ #} selfPtr argsPtr kwargsPtr
	>>= stealObject

-- | Call a callable Python object /self/, with arguments given by the list.
callArgs :: Object self => self -> [SomeObject] -> IO SomeObject
callArgs self args = do
	args' <- Tuple.toTuple args
	D.new >>= call self args'

-- | Call the named method of object /self/, with arguments given by the
-- tuple and named arguments given by the dictionary. Returns the result of
-- the call on success, or throws an exception on failure. This is the
-- equivalent of the Python expression @self.method(args)@.
callMethod :: Object self => self -> T.Text -> Tuple -> Dictionary -> IO SomeObject
callMethod self name args kwargs = do
	method <- getAttribute self =<< U.toUnicode name
	call method args kwargs

-- | Call the named method of object /self/, with arguments given by the
-- list. Returns the result of the call on success, or throws an exception
-- on failure. This is the equivalent of the Python expression
-- @self.method(args)@.
callMethodArgs :: Object self => self -> T.Text -> [SomeObject] -> IO SomeObject
callMethodArgs self name args = do
	args' <- Tuple.toTuple args
	D.new >>= callMethod self name args'

data Comparison = LT | LE | EQ | NE | GT | GE
	deriving (Show)

comparisonToInt :: Comparison -> CInt
comparisonToInt = fromIntegral . fromEnum . enum where
	enum LT = HSCPYTHON_LT
	enum LE = HSCPYTHON_LE
	enum EQ = HSCPYTHON_EQ
	enum NE = HSCPYTHON_NE
	enum GT = HSCPYTHON_GT
	enum GE = HSCPYTHON_GE

-- | Compare the values of /a/ and /b/ using the specified comparison.
-- If an exception is raised, throws an exception.
{# fun PyObject_RichCompareBool as richCompare
	`(Object a, Object b)' =>
	{ withObject* `a'
	, withObject* `b'
	, comparisonToInt `Comparison'
	} -> `Bool' checkBoolReturn* #}

-- | Returns 'True' if the object /self/ is considered to be true, and 'False'
-- otherwise. This is equivalent to the Python expression @not not self@. On
-- failure, throws an exception.
{# fun PyObject_IsTrue as toBool
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

-- | Compute and return the hash value of an object /self/. On failure,
-- throws an exception. This is the equivalent of the Python expression
-- @hash(self)@.
{# fun PyObject_Hash as hash
	`Object self' =>
	{ withObject* `self'
	} -> `Integer' checkIntReturn* #}

-- | This is equivalent to the Python expression @dir(self)@, returning a
-- (possibly empty) list of strings appropriate for the object argument,
-- or throws an exception if there was an error.
{# fun PyObject_Dir as dir
	`Object self' =>
	{ withObject* `self'
	} -> `List' stealObject* #}

-- | This is equivalent to the Python expression @iter(self)@. It returns a
-- new iterator for the object argument, or the object itself if the object
-- is already an iterator. Throws @TypeError@ if the object cannot be
-- iterated.
{# fun PyObject_GetIter as getIterator
	`Object self' =>
	{ withObject* `self'
	} -> `SomeObject' stealObject* #}
