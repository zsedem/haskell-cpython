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

module CPython.Types.Complex
  ( Complex
  , complexType
  , toComplex
  , fromComplex
  ) where

#include <hscpython-shim.h>

import qualified Data.Complex as C

import           CPython.Internal

newtype Complex = Complex (ForeignPtr Complex)

instance Object Complex where
  toObject (Complex x) = SomeObject x
  fromForeignPtr = Complex

instance Concrete Complex where
  concreteType _ = complexType

{# fun pure unsafe hscpython_PyComplex_Type as complexType
  {} -> `Type' peekStaticObject* #}

toComplex :: C.Complex Double -> IO Complex
toComplex x = raw >>= stealObject where
  real = realToFrac $ C.realPart x
  imag = realToFrac $ C.imagPart x
  raw = {# call PyComplex_FromDoubles as ^ #} real imag

fromComplex :: Complex -> IO (C.Complex Double)
fromComplex py = withObject py $ \pyPtr -> do
  real <- {# call PyComplex_RealAsDouble as ^ #} pyPtr
  imag <- {# call PyComplex_ImagAsDouble as ^ #} pyPtr
  return $ realToFrac real C.:+ realToFrac imag
