{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CPython.Simple.Instances where

import Control.Exception (Exception(..), throwIO)
import Control.Monad ((<=<))
import Data.Text (Text)
import qualified Data.Text as T

import qualified CPython.Constants as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Tuple as Py (fromTuple)

-- TODO: ToPy/FromPy for Bool will require some library changes (e.g. adding fromBool)

class ToPy a where
  toPy :: a -> IO Py.SomeObject

class FromPy a where
  fromPy :: Py.SomeObject -> IO a

data PyCastException = PyCastException String
  deriving (Show)

instance Exception PyCastException where
  displayException (PyCastException typename) =
    "FromPy could not cast to " ++ typename

easyToPy
  :: Py.Object c
  => (a -> IO c) -- ^ python to- conversion, e.g. Py.toFloat
  -> a           -- ^ haskell type being converted
  -> IO Py.SomeObject
easyToPy f = fmap Py.toObject . f

easyFromPy
  :: Py.Concrete b
  => (b -> IO c)   -- ^ python from- conversion, e.g. Py.fromFloat
  -> Text          -- ^ error message text for the haskell type being converted to, e.g. "Double"
  -> Py.SomeObject -- ^ python object to cast from
  -> IO c
easyFromPy conversion typename obj = do
  casted <- Py.cast obj
  case casted of
    Nothing -> throwIO $ PyCastException (T.unpack typename)
    Just x -> conversion x

instance ToPy Integer where
  toPy = easyToPy Py.toInteger

instance FromPy Integer where
  fromPy = easyFromPy Py.fromInteger "Integer"

instance ToPy Double where
  toPy = easyToPy Py.toFloat

instance FromPy Double where
  fromPy = easyFromPy Py.fromFloat "Double"

instance ToPy Text where
  toPy = easyToPy Py.toUnicode

instance FromPy Text where
  fromPy = easyFromPy Py.fromUnicode "Text"

instance ToPy Char where
  toPy = easyToPy Py.toUnicode . T.singleton

instance FromPy Char where
  fromPy c = T.head <$> easyFromPy Py.fromUnicode "Char" c

instance ToPy String where
  toPy = easyToPy Py.toUnicode . T.pack

instance FromPy String where
  fromPy s = T.unpack <$> easyFromPy Py.fromUnicode "String" s

instance (FromPy a, FromPy b) => FromPy (a, b) where
  fromPy val = do
    [pyA, pyB] <- easyFromPy Py.fromTuple "(a, b)" val
    a <- fromPy pyA
    b <- fromPy pyB
    pure (a, b)

instance (FromPy a, FromPy b, FromPy c) => FromPy (a, b, c) where
  fromPy val = do
    [pyA, pyB, pyC] <- easyFromPy Py.fromTuple "(a, b, c)" val
    a <- fromPy pyA
    b <- fromPy pyB
    c <- fromPy pyC
    pure (a, b, c)

instance FromPy a => FromPy (Maybe a) where
  fromPy val = do
    isNone <- Py.isNone val
    if isNone
      then pure Nothing
      else Just <$> fromPy val

instance ToPy a => ToPy (Maybe a) where
  toPy Nothing = Py.none
  toPy (Just a) = toPy a

instance FromPy a => FromPy [a] where
  fromPy val = do
    list <- easyFromPy Py.fromList "[a]" val
    mapM fromPy list

instance ToPy a => ToPy [a] where
  toPy val = do
    list <- mapM toPy val
    Py.toObject <$> Py.toList list

instance FromPy () where
  fromPy _ = pure ()
