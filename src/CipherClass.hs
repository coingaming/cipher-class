{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CipherClass
  ( Encrypted (..),
    Encryptable (..),
    CipherM (..),
    CryptoFailable (..),
    AES256,
    IV,
    cipherInit,
    makeIV,
    reType,
    getRandomBytes,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher, IV, cipherInit, ctrCombine, makeIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.Random (getRandomBytes)
import Data.ByteString as BS (ByteString)
import Data.Coerce (coerce)
import qualified Data.Text.Encoding as TE (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy as LT (Text, fromStrict, toStrict)
import Database.Esqueleto (PersistField, PersistFieldSql)
import Universum

newtype Encrypted a b
  = Encrypted b
  deriving (PersistField, PersistFieldSql)

class Encryptable a b where
  encrypt :: (BlockCipher c) => c -> IV c -> a -> Encrypted a b
  decrypt :: (BlockCipher c) => c -> IV c -> Encrypted a b -> Either UnicodeException a

class CipherM m where
  encryptM :: (Encryptable a b) => a -> m (Encrypted a b)
  decryptM :: (Encryptable a b) => Encrypted a b -> m (Either UnicodeException a)

instance Encryptable ByteString ByteString where
  encrypt c i = Encrypted . ctrCombine c i
  decrypt c i = Right . ctrCombine c i . coerce

instance Encryptable Text ByteString where
  encrypt c i x =
    reType (encrypt c i $ TE.encodeUtf8 x :: Encrypted ByteString ByteString)
  decrypt c i x =
    (decrypt c i $ reType x :: Either UnicodeException ByteString)
      >>= TE.decodeUtf8'

instance Encryptable LT.Text ByteString where
  encrypt c i x =
    reType
      (encrypt c i $ LT.toStrict x :: Encrypted Text ByteString)
  decrypt c i x =
    second
      LT.fromStrict
      (decrypt c i $ reType x :: Either UnicodeException Text)

instance (Traversable f, Encryptable a b) => Encryptable (f a) (f b) where
  encrypt c i xs =
    Encrypted $
      (coerce . (encrypt c i :: a -> Encrypted a b) :: a -> b) <$> xs
  decrypt c i xs =
    mapM (decrypt c i . Encrypted) (coerce xs :: f b)

reType :: Encrypted b a -> Encrypted c a
reType = Encrypted . coerce
