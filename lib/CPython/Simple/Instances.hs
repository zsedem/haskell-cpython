{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CPython.Simple.Instances where

import Control.Exception (Exception(..), throwIO)
import Control.Monad ((<=<))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import qualified CPython.Constants as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types as Py
import qualified CPython.Types.Tuple as Py (fromTuple)

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
  :: (Py.Concrete b, Typeable c)
  => (b -> IO c)   -- ^ python from- conversion, e.g. Py.fromFloat
  -> Proxy c       -- ^ proxy for the type being converted to
  -> Py.SomeObject -- ^ python object to cast from
  -> IO c
easyFromPy conversion typename obj = do
  casted <- Py.cast obj
  case casted of
    Nothing -> throwIO $ PyCastException (show $ typeRep typename)
    Just x -> conversion x

instance ToPy Bool where
  toPy b = if b then Py.true else Py.false

instance FromPy Bool where
  fromPy pyB = do
    isTrue <- Py.isTrue pyB
    isFalse <- Py.isFalse pyB
    case (isTrue, isFalse) of
      (True, False) -> pure True
      (False, True) -> pure False
      (False, False) -> throwIO . PyCastException . show $ typeRep (Proxy :: Proxy Bool)
      (True, True) -> throwIO . PyCastException $ (show $ typeRep (Proxy :: Proxy Bool)) ++
        ". Python object was True and False at the same time. Should be impossible."

instance ToPy Integer where
  toPy = easyToPy Py.toInteger

instance FromPy Integer where
  fromPy = easyFromPy Py.fromInteger Proxy

instance ToPy Double where
  toPy = easyToPy Py.toFloat

instance FromPy Double where
  fromPy = easyFromPy Py.fromFloat Proxy

instance ToPy Text where
  toPy = easyToPy Py.toUnicode

instance FromPy Text where
  fromPy = easyFromPy Py.fromUnicode Proxy

instance ToPy Char where
  toPy = easyToPy Py.toUnicode . T.singleton

instance FromPy Char where
  fromPy c = T.head <$> easyFromPy Py.fromUnicode Proxy c

instance ToPy String where
  toPy = easyToPy Py.toUnicode . T.pack

instance FromPy String where
  fromPy s = T.unpack <$> easyFromPy Py.fromUnicode Proxy s

instance (FromPy a, FromPy b) => FromPy (a, b) where
  fromPy val = do
    [pyA, pyB] <- easyFromPy Py.fromTuple Proxy val
    a <- fromPy pyA
    b <- fromPy pyB
    pure (a, b)

instance (ToPy a, ToPy b) => ToPy (a, b) where
  toPy (a, b) = do
    pyA <- toPy a
    pyB <- toPy b
    easyToPy Py.toTuple [pyA, pyB]

instance (FromPy a, FromPy b, FromPy c) => FromPy (a, b, c) where
  fromPy val = do
    [pyA, pyB, pyC] <- easyFromPy Py.fromTuple Proxy val
    a <- fromPy pyA
    b <- fromPy pyB
    c <- fromPy pyC
    pure (a, b, c)

instance (ToPy a, ToPy b, ToPy c) => ToPy (a, b, c) where
  toPy (a, b, c) = do
    pyA <- toPy a
    pyB <- toPy b
    pyC <- toPy c
    easyToPy Py.toTuple [pyA, pyB, pyC]

instance (FromPy a, FromPy b, FromPy c, FromPy d) => FromPy (a, b, c, d) where
  fromPy val = do
    [pyA, pyB, pyC, pyD] <- easyFromPy Py.fromTuple Proxy val
    a <- fromPy pyA
    b <- fromPy pyB
    c <- fromPy pyC
    d <- fromPy pyD
    pure (a, b, c, d)

instance (ToPy a, ToPy b, ToPy c, ToPy d) => ToPy (a, b, c, d) where
  toPy (a, b, c, d) = do
    pyA <- toPy a
    pyB <- toPy b
    pyC <- toPy c
    pyD <- toPy d
    easyToPy Py.toTuple [pyA, pyB, pyC, pyD]

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
    list <- easyFromPy Py.fromList Proxy val
    mapM fromPy list

instance ToPy a => ToPy [a] where
  toPy val = do
    list <- mapM toPy val
    Py.toObject <$> Py.toList list

instance FromPy () where
  fromPy _ = pure ()
