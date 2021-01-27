{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
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

module CPython.Internal
  (
  -- * FFI support
    module Foreign
  , module Foreign.C
  , cToBool
  , cFromBool
  , peekText
  , peekTextW
  , peekMaybeTextW
  , withText
  , withTextW
  , withMaybeTextW
  , mapWith
  , unsafePerformIO
  
  -- * Fundamental types
  , SomeObject (..)
  , Type (..)
  , Dictionary (..)
  , List (..)
  , Tuple (..)
  
  -- * Objects
  , Object (..)
  , Concrete (..)
  , withObject
  , peekObject
  , peekStaticObject
  , stealObject
  , incref
  , decref
  , callObjectRaw
  , unsafeCast
  -- Threading
  , saveThread
  , restoreThread
  -- * Exceptions
  , Exception (..)
  , exceptionIf
  , checkStatusCode
  , checkBoolReturn
  , checkIntReturn
  
  -- * Other classes
  -- ** Mapping
  , Mapping (..)
  , SomeMapping (..)
  , unsafeCastToMapping
  
  -- ** Sequence
  , Sequence (..)
  , SomeSequence (..)
  , unsafeCastToSequence
  
  -- ** Iterator
  , Iterator (..)
  , SomeIterator (..)
  , unsafeCastToIterator
  ) where

#include <hscpython-shim.h>

import qualified Control.Exception as E
import           Control.Concurrent.Chan
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Foreign hiding (newForeignPtr, newForeignPtr_)
import           Foreign.C
import           Foreign.Concurrent(newForeignPtr)
import           System.IO.Unsafe (unsafePerformIO)

cToBool :: CInt -> Bool
cToBool = (/= 0)

cFromBool :: Bool -> CInt
cFromBool x = if x then 1 else 0

peekText :: CString -> IO T.Text
peekText = fmap T.pack . peekCString

peekTextW :: CWString -> IO T.Text
peekTextW = fmap T.pack . peekCWString

peekMaybeTextW :: CWString -> IO (Maybe T.Text)
peekMaybeTextW = maybePeek peekTextW

withText :: T.Text -> (CString -> IO a) -> IO a
withText = withCString . T.unpack

withTextW :: T.Text -> (CWString -> IO a) -> IO a
withTextW = withCWString . T.unpack

withMaybeTextW :: Maybe T.Text -> (CWString -> IO a) -> IO a
withMaybeTextW = maybeWith withTextW

mapWith :: (a -> (b -> IO c) -> IO c) -> [a] -> ([b] -> IO c) -> IO c
mapWith with' = step [] where
  step acc [] io = io acc
  step acc (x:xs) io = with' x $ \y -> step (acc ++ [y]) xs io

data SomeObject = forall a. (Object a) => SomeObject (ForeignPtr a)

class Object a where
  toObject :: a -> SomeObject
  fromForeignPtr :: ForeignPtr a -> a

class Object a => Concrete a where
  concreteType :: a -> Type

instance Object SomeObject where
  toObject = id
  fromForeignPtr = SomeObject

newtype Type = Type (ForeignPtr Type)
instance Object Type where
  toObject (Type x) = SomeObject x
  fromForeignPtr = Type

newtype Dictionary = Dictionary (ForeignPtr Dictionary)
instance Object Dictionary where
  toObject (Dictionary x) = SomeObject x
  fromForeignPtr = Dictionary

newtype List = List (ForeignPtr List)
instance Object List where
  toObject (List x) = SomeObject x
  fromForeignPtr = List

newtype Tuple = Tuple (ForeignPtr Tuple)
instance Object Tuple where
  toObject (Tuple x) = SomeObject x
  fromForeignPtr = Tuple

withObject :: Object obj => obj -> (Ptr a -> IO b) -> IO b
withObject obj io = case toObject obj of
  SomeObject ptr -> withForeignPtr ptr (io . castPtr)

peekObject :: Object obj => Ptr a -> IO obj
peekObject ptr = E.bracketOnError incPtr decref mkObj where
  incPtr = incref ptr >> return ptr
  mkObj _ = fromForeignPtr <$> newForeignPtr (castPtr ptr) (decref ptr)

peekStaticObject :: Object obj => Ptr a -> IO obj
peekStaticObject ptr = fromForeignPtr <$> newForeignPtr_ (castPtr ptr)
  where
    newForeignPtr_ p = newForeignPtr p (return ())



unsafeStealObject :: Object obj => Ptr a -> IO obj
unsafeStealObject ptr = fromForeignPtr <$> newForeignPtr (castPtr ptr) (decref ptr)

stealObject :: Object obj => Ptr a -> IO obj
stealObject ptr = exceptionIf (ptr == nullPtr) >> unsafeStealObject ptr

incref :: Ptr a -> IO ()
incref ptr = do
  restoreThread
  incref' ptr
  saveThread
{# fun hscpython_Py_INCREF as incref'
  { castPtr `Ptr a'
  } -> `()' id #}


decref :: Ptr a -> IO ()
decref ptr = do
  restoreThread
  decref' ptr
  saveThread
{# fun hscpython_Py_DECREF as decref'
  { castPtr `Ptr a'
  } -> `()' id #}

{-# NOINLINE pythonThreadStates #-}
pythonThreadStates :: Chan (Ptr ())  -- Who doesn't like global variables :)
pythonThreadStates = unsafePerformIO newChan

saveThread :: IO ()
saveThread = do
  state <- {# call PyEval_SaveThread as ^ #}
  writeChan pythonThreadStates state

restoreThread :: IO ()
restoreThread = do
  state <- readChan pythonThreadStates
  {# call PyEval_RestoreThread as ^ #} state


{# fun PyObject_CallObject as callObjectRaw
  `(Object self, Object args)' =>
  { withObject* `self'
  , withObject* `args'
  } -> `SomeObject' stealObject* #}

unsafeCast :: (Object a, Object b) => a -> b
unsafeCast a = case toObject a of
  SomeObject ptr -> fromForeignPtr (castForeignPtr ptr)

data Exception = Exception
  { exceptionType      :: SomeObject
  , exceptionValue     :: SomeObject
  , exceptionTraceback :: Maybe SomeObject
  }
  deriving (Typeable)

instance Show Exception where
  show _ = "<CPython exception>"

instance E.Exception Exception

exceptionIf :: Bool -> IO ()
exceptionIf False = return ()
exceptionIf True =
  alloca $ \pType ->
  alloca $ \pValue ->
  alloca $ \pTrace -> do
    {# call PyErr_Fetch as ^ #} pType pValue pTrace
    {# call PyErr_NormalizeException as ^ #} pType pValue pTrace
    eType <- unsafeStealObject =<< peek pType
    eValue <- unsafeStealObject =<< peek pValue
    eTrace <- maybePeek unsafeStealObject =<< peek pTrace
    E.throwIO $ Exception eType eValue eTrace

checkStatusCode :: CInt -> IO ()
checkStatusCode = exceptionIf . (== -1)

checkBoolReturn :: CInt -> IO Bool
checkBoolReturn x = do
  exceptionIf $ x == -1
  return $ x /= 0

checkIntReturn :: Integral a => a -> IO Integer
checkIntReturn x = do
  exceptionIf $ x == -1
  return $ toInteger x

data SomeMapping = forall a. (Mapping a) => SomeMapping (ForeignPtr a)

class Object a => Mapping a where
  toMapping :: a -> SomeMapping

instance Object SomeMapping where
  toObject (SomeMapping x) = SomeObject x
  fromForeignPtr = SomeMapping

instance Mapping SomeMapping where
  toMapping = id

unsafeCastToMapping :: Object a => a -> SomeMapping
unsafeCastToMapping x = case toObject x of
  SomeObject ptr -> let
    ptr' = castForeignPtr ptr :: ForeignPtr SomeMapping
    in SomeMapping ptr'

data SomeSequence = forall a. (Sequence a) => SomeSequence (ForeignPtr a)

class Object a => Sequence a where
  toSequence :: a -> SomeSequence

instance Object SomeSequence where
  toObject (SomeSequence x) = SomeObject x
  fromForeignPtr = SomeSequence

instance Sequence SomeSequence where
  toSequence = id

unsafeCastToSequence :: Object a => a -> SomeSequence
unsafeCastToSequence x = case toObject x of
  SomeObject ptr -> let
    ptr' = castForeignPtr ptr :: ForeignPtr SomeSequence
    in SomeSequence ptr'

data SomeIterator = forall a. (Iterator a) => SomeIterator (ForeignPtr a)

class Object a => Iterator a where
  toIterator :: a -> SomeIterator

instance Object SomeIterator where
  toObject (SomeIterator x) = SomeObject x
  fromForeignPtr = SomeIterator

instance Iterator SomeIterator where
  toIterator = id

unsafeCastToIterator :: Object a => a -> SomeIterator
unsafeCastToIterator x = case toObject x of
  SomeObject ptr -> let
    ptr' = castForeignPtr ptr :: ForeignPtr SomeIterator
    in SomeIterator ptr'
