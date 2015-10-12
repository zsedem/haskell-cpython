{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}

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

module CPython.Protocols.Number
	( Number (..)
	, SomeNumber
	, castToNumber
	, add
	, subtract
	, multiply
	, floorDivide
	, trueDivide
	, remainder
	, divmod
	, power
	, negative
	, positive
	, absolute
	, invert
	, shiftL
	, shiftR
	, and
	, xor
	, or
	, inPlaceAdd
	, inPlaceSubtract
	, inPlaceMultiply
	, inPlaceFloorDivide
	, inPlaceTrueDivide
	, inPlaceRemainder
	, inPlacePower
	, inPlaceShiftL
	, inPlaceShiftR
	, inPlaceAnd
	, inPlaceXor
	, inPlaceOr
	, toInteger
	, toFloat
	, toBase
	) where

#include <hscpython-shim.h>

import           Prelude hiding (Integer, Float, subtract, and, or, toInteger)
import qualified Prelude as Prelude

import           CPython.Constants (none)
import           CPython.Internal hiding (xor, shiftR, shiftL)
import           CPython.Types.Complex (Complex)
import           CPython.Types.Float (Float)
import           CPython.Types.Integer (Integer)
import           CPython.Types.Set (Set, FrozenSet)
import           CPython.Types.Unicode (Unicode)

data SomeNumber = forall a. (Number a) => SomeNumber (ForeignPtr a)

class Object a => Number a where
	toNumber :: a -> SomeNumber

instance Object SomeNumber where
	toObject (SomeNumber x) = SomeObject x
	fromForeignPtr = SomeNumber

instance Number SomeNumber where
	toNumber = id

instance Number Integer where
	toNumber = unsafeCastToNumber

instance Number Float where
	toNumber = unsafeCastToNumber

instance Number Complex where
	toNumber = unsafeCastToNumber

-- lol wut
instance Number Set where
	toNumber = unsafeCastToNumber

instance Number FrozenSet where
	toNumber = unsafeCastToNumber

unsafeCastToNumber :: Object a => a -> SomeNumber
unsafeCastToNumber x = case toObject x of
	SomeObject ptr -> let
		ptr' = castForeignPtr ptr :: ForeignPtr SomeNumber
		in SomeNumber ptr'

castToNumber :: Object a => a -> IO (Maybe SomeNumber)
castToNumber obj =
	withObject obj $ \objPtr -> do
	isNumber <- fmap cToBool $ {# call PyNumber_Check as ^ #} objPtr
	return $ if isNumber
		then Just $ unsafeCastToNumber obj
		else Nothing

add :: (Number a, Number b) => a -> b -> IO SomeNumber
add = c_add

-- c2hs won't accept functions named "add" any more, so have it generate
-- c_add and then wrap that manually.
{# fun PyNumber_Add as c_add
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Subtract as subtract
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Multiply as multiply
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_FloorDivide as floorDivide
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_TrueDivide as trueDivide
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Remainder as remainder
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Divmod as divmod
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

power :: (Number a, Number b, Number c) => a -> b -> Maybe c -> IO SomeNumber
power a b mc =
	withObject a $ \aPtr ->
	withObject b $ \bPtr ->
	maybe none (return . toObject) mc >>= \c ->
	withObject c $ \cPtr ->
	{# call PyNumber_Power as ^ #} aPtr bPtr cPtr
	>>= stealObject

{# fun PyNumber_Negative as negative
	`Number a' =>
	{ withObject* `a'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Positive as positive
	`Number a' =>
	{ withObject* `a'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Absolute as absolute
	`Number a' =>
	{ withObject* `a'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Invert as invert
	`Number a' =>
	{ withObject* `a'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Lshift as shiftL
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Rshift as shiftR
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_And as and
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Xor as xor
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Or as or
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceAdd as inPlaceAdd
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceSubtract as inPlaceSubtract
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceMultiply as inPlaceMultiply
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceFloorDivide as inPlaceFloorDivide
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceTrueDivide as inPlaceTrueDivide
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceRemainder as inPlaceRemainder
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

inPlacePower ::(Number a, Number b, Number c) => a -> b -> Maybe c -> IO SomeNumber
inPlacePower a b mc =
	withObject a $ \aPtr ->
	withObject b $ \bPtr ->
	maybe none (return . toObject) mc >>= \c ->
	withObject c $ \cPtr ->
	{# call PyNumber_InPlacePower as ^ #} aPtr bPtr cPtr
	>>= stealObject

{# fun PyNumber_InPlaceLshift as inPlaceShiftL
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceRshift as inPlaceShiftR
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceAnd as inPlaceAnd
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceXor as inPlaceXor
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_InPlaceOr as inPlaceOr
	`(Number a, Number b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeNumber' stealObject* #}

{# fun PyNumber_Long as toInteger
	`Number a' =>
	{ withObject* `a'
	} -> `Integer' stealObject* #}

{# fun PyNumber_Float as toFloat
	`Number a' =>
	{ withObject* `a'
	} -> `Float' stealObject* #}

{# fun PyNumber_ToBase as toBase
	`Number a' =>
	{ withObject* `a'
	, fromIntegral `Prelude.Integer'
	} -> `Unicode' stealObject* #}
