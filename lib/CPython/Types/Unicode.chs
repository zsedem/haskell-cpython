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

module CPython.Types.Unicode
  (
  -- * Unicode objects
    Unicode
  , Encoding
  , ErrorHandling (..)
  , unicodeType
  , toUnicode
  , fromUnicode
  , length
  , fromEncodedObject
  , fromObject
  , encode
  , decode
  
  -- * Methods and slot functions
  , append
  , split
  , splitLines
  , translate
  , join
  , MatchDirection (..)
  , tailMatch
  , FindDirection (..)
  , find
  , count
  , replace
  , format
  , contains
  ) where

#include <hscpython-shim.h>

import           Prelude hiding (length)
import           Control.Exception (ErrorCall (..), throwIO)
import qualified Data.Text as T
import           Foreign.C.String
import           Foreign.C.Types
import           CPython.Internal
import           CPython.Types.Bytes (Bytes)

newtype Unicode = Unicode (ForeignPtr Unicode)

instance Object Unicode where
  toObject (Unicode x) = SomeObject x
  fromForeignPtr = Unicode

instance Concrete Unicode where
  concreteType _ = unicodeType

type Encoding = T.Text
data ErrorHandling
  = Strict
  | Replace
  | Ignore
  deriving (Show, Eq)

withErrors :: ErrorHandling -> (CString -> IO a) -> IO a
withErrors errors = withCString $ case errors of
  Strict -> "strict"
  Replace -> "replace"
  Ignore -> "ignore"

{# fun pure unsafe hscpython_PyUnicode_Type as unicodeType
  {} -> `Type' peekStaticObject* #}


toUnicode :: T.Text -> IO Unicode
toUnicode txt = withCWStringLen (T.unpack txt) $ \(wstr, sz) -> do
  obj <- {# call hscpython_PyUnicode_FromUnicode #} (castPtr wstr) (fromIntegral sz)
  stealObject obj


fromUnicode :: Unicode -> IO T.Text
fromUnicode obj = withObject obj $ \ptr -> do
  wstrPtr <- {# call hscpython_PyUnicode_AsUnicode #} ptr
  wstr <- peekCWString . castPtr $ wstrPtr
  return . T.pack $ wstr

{# fun hscpython_PyUnicode_GetSize as length
  { withObject* `Unicode'
  } -> `Integer' checkIntReturn* #}


-- | Coerce an encoded object /obj/ to an Unicode object.
--
-- 'Bytes' and other char buffer compatible objects are decoded according to
-- the given encoding and error handling mode.
--
-- All other objects, including 'Unicode' objects, cause a @TypeError@ to be
-- thrown.
{# fun hscpython_PyUnicode_FromEncodedObject as fromEncodedObject
  `Object obj' =>
  { withObject* `obj'
  , withText* `Encoding'
  , withErrors* `ErrorHandling'
  } -> `Unicode' stealObject* #}

-- | Shortcut for @'fromEncodedObject' \"utf-8\" 'Strict'@
fromObject :: Object obj => obj -> IO Unicode
fromObject obj = fromEncodedObject obj (T.pack "utf-8") Strict

-- | Encode a 'Unicode' object and return the result as 'Bytes' object.
-- The encoding and error mode have the same meaning as the parameters of
-- the the @str.encode()@ method. The codec to be used is looked up using
-- the Python codec registry.
{# fun hscpython_PyUnicode_AsEncodedString as encode
  { withObject* `Unicode'
  , withText* `Encoding'
  , withErrors* `ErrorHandling'
  } -> `Bytes' stealObject* #}

-- | Create a 'Unicode' object by decoding a 'Bytes' object. The encoding and
-- error mode have the same meaning as the parameters of the the
-- @str.encode()@ method. The codec to be used is looked up using the Python
-- codec registry.
decode :: Bytes -> Encoding -> ErrorHandling -> IO Unicode
decode bytes enc errors =
  withObject bytes $ \bytesPtr ->
  withText enc $ \encPtr ->
  withErrors errors $ \errorsPtr ->
  alloca $ \bufferPtr ->
  alloca $ \lenPtr -> do
  {# call PyBytes_AsStringAndSize as ^ #} bytesPtr bufferPtr lenPtr
    >>= checkStatusCode
  buffer <- peek bufferPtr
  len <- peek lenPtr
  {# call hscpython_PyUnicode_Decode #} buffer len encPtr errorsPtr
  >>= stealObject

{# fun hscpython_PyUnicode_Concat as append
  { withObject* `Unicode'
  , withObject* `Unicode'
  } -> `Unicode' stealObject* #}

-- | Split a string giving a 'List' of 'Unicode' objects. If the separator is
-- 'Nothing', splitting will be done at all whitespace substrings. Otherwise,
-- splits occur at the given separator. Separators are not included in the
-- resulting list.
split
  :: Unicode
  -> Maybe Unicode -- ^ Separator
  -> Maybe Integer -- ^ Maximum splits
  -> IO List
split s sep maxsplit =
  withObject s $ \sPtr ->
  maybeWith withObject sep $ \sepPtr ->
  let max' = maybe (- 1) fromInteger maxsplit in
  {# call hscpython_PyUnicode_Split #} sPtr sepPtr max'
  >>= stealObject

-- | Split a 'Unicode' string at line breaks, returning a list of 'Unicode'
-- strings. CRLF is considered to be one line break. If the second parameter
-- is 'False', the line break characters are not included in the resulting
-- strings.
{# fun hscpython_PyUnicode_Splitlines as splitLines
  { withObject* `Unicode'
  , `Bool'
  } -> `List' stealObject* #}

-- | Translate a string by applying a character mapping table to it.
--
-- The mapping table must map Unicode ordinal integers to Unicode ordinal
-- integers or @None@ (causing deletion of the character).
--
-- Mapping tables need only provide the @__getitem__()@ interface;
-- dictionaries and sequences work well. Unmapped character ordinals (ones
-- which cause a @LookupError@) are left untouched and are copied as-is.
--
-- The error mode has the usual meaning for codecs.
{# fun hscpython_PyUnicode_Translate as translate
  `Object table' =>
  { withObject* `Unicode'
  , withObject* `table'
  , withErrors* `ErrorHandling'
  } -> `Unicode' stealObject* #}

-- | Join a sequence of strings using the given separator.
{# fun hscpython_PyUnicode_Join as join
  `Sequence seq' =>
  { withObject* `Unicode'
  , withObject* `seq'
  } -> `Unicode' stealObject* #}

data MatchDirection = Prefix | Suffix
  deriving (Show, Eq)

-- | Return 'True' if the substring matches @string*[*start:end]@ at the
-- given tail end (either a 'Prefix' or 'Suffix' match), 'False' otherwise.
tailMatch
  :: Unicode -- ^ String
  -> Unicode -- ^ Substring
  -> Integer -- ^ Start
  -> Integer -- ^ End
  -> MatchDirection
  -> IO Bool
tailMatch str substr start end dir =
  withObject str $ \strPtr ->
    withObject substr $ \substrPtr ->
      let start' = fromInteger start
          end' = fromInteger end
          dir' = case dir of Prefix -> -1
                             Suffix -> 1
      in {# call hscpython_PyUnicode_Tailmatch #} strPtr substrPtr start' end' dir'
  >>= checkBoolReturn

data FindDirection = Forwards | Backwards
  deriving (Show, Eq)

-- | Return the first position of the substring in @string*[*start:end]@
-- using the given direction. The return value is the index of the first
-- match; a value of 'Nothing' indicates that no match was found.
find
  :: Unicode -- ^ String
  -> Unicode -- ^ Substring
  -> Integer -- ^ Start
  -> Integer -- ^ End
  -> FindDirection
  -> IO (Maybe Integer)
find str substr start end dir =
  withObject str $ \strPtr ->
    withObject substr $ \substrPtr ->
      let start' = fromInteger start
          end' = fromInteger end
          dir' = case dir of Forwards -> 1
                             Backwards -> -1
      in do
        cRes <- {# call hscpython_PyUnicode_Find #} strPtr substrPtr start' end' dir'
        exceptionIf $ cRes == -2
        case cRes of
          -1 -> return Nothing
          x | x >= 0 -> return . Just . toInteger $ x
          x -> throwIO . ErrorCall $ "Invalid return code: " ++ show x

-- | Return the number of non-overlapping occurrences of the substring in
-- @string[start:end]@.
count
  :: Unicode -- ^ String
  -> Unicode -- ^ Substring
  -> Integer -- ^ Start
  -> Integer -- ^ End
  -> IO Integer
count str substr start end =
  withObject str $ \str' ->
  withObject substr $ \substr' ->
  let start' = fromInteger start in
  let end' = fromInteger end in
  {# call hscpython_PyUnicode_Count #} str' substr' start' end'
  >>= checkIntReturn

-- | Replace occurrences of the substring with a given replacement. If the
-- maximum count is 'Nothing', replace all occurences.
replace
  :: Unicode -- ^ String
  -> Unicode -- ^ Substring
  -> Unicode -- ^ Replacement
  -> Maybe Integer -- ^ Maximum count
  -> IO Unicode
replace str substr replstr maxcount =
  withObject str $ \strPtr ->
    withObject substr $ \substrPtr ->
      withObject replstr $ \replstrPtr ->
        let maxcount' = case maxcount of Nothing -> -1
                                         Just x -> fromInteger x 
        in {# call hscpython_PyUnicode_Replace #} strPtr substrPtr replstrPtr maxcount'
  >>= stealObject

-- | Return a new 'Unicode' object from the given format and args; this is
-- analogous to @format % args@.
{# fun hscpython_PyUnicode_Format as format
  { withObject* `Unicode'
  , withObject* `Tuple'
  } -> `Unicode' stealObject* #}

-- | Check whether /element/ is contained in a string.
--
-- /element/ has to coerce to a one element string.
{# fun hscpython_PyUnicode_Contains as contains
  `Object element' =>
  { withObject* `Unicode'
  , withObject* `element'
  } -> `Bool' checkBoolReturn* #}
