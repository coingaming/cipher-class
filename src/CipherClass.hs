{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Data encryption is common security-related practice in
-- database usage. One of the negative side effects of encryption is
-- that typed data in its encrypted form becames untyped and
-- usually exists in form of 'ByteString' or similar blind type.
-- Operating with untyped data is very error-prone and should be avoided.
-- This library proposes the way to fix it.
--
-- Let's have an example of 'User' sum type where his 'Login'
-- is not sensitive type, but 'Address' is sensitive.
-- It should never be shown and should be stored only in
-- encrypted form.
--
-- @
-- newtype Login
--   = Login Text
--   deriving newtype (Eq, Arbitrary, Show, PersistField, PersistFieldSql)
--
-- newtype Address
--   = Address Text
--   deriving newtype (Eq, Arbitrary)
--
-- instance Show Address where
--   show = const \"SECRET\"
-- @
--
-- Now let's implement 'Encryptable' class for 'Address' type -
-- we will store it as encrypted 'ByteString'.
-- After decryption 'UnicodeException' can be raised because
-- 'Address' is newtype around 'Text' - we will express it
-- in implementation as well.
--
-- @
-- instance Encryptable Address ByteString UnicodeException where
--   encrypt c i x = reType $ encrypt c i (coerce x :: Text)
--   decrypt c i = second Address . decrypt c i . reType
-- @
module CipherClass
  ( -- * Type
    Encrypted (..),

    -- * Class
    Encryptable (..),
    Encryptor (..),

    -- * Utility
    reType,

    -- * Re-export
    CryptoFailable (..),
    BlockCipher,
    AES256,
    IV,
    cipherInit,
    makeIV,
    getRandomBytes,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher, IV, cipherInit, ctrCombine, makeIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict)
import Data.Coerce (coerce)
import qualified Data.Text.Encoding as TE (decodeUtf8', encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, fromStrict, toStrict)
import Database.Esqueleto (PersistField, PersistFieldSql)
import Universum

-- | Value of this type represents
-- value of type __a__ (phantom) encrypted in form of
-- value of type __b__ (non-phantom) which can cause
-- error of type __e__ (phantom) when construction of
-- value of type __a__ fails after decryption.
newtype Encrypted b e a = Encrypted b
  deriving (PersistField, PersistFieldSql)

-- | Class represents the idea of
-- typed symmetric encryption and decryption
class Encryptable b e a where
  encrypt :: (BlockCipher c) => c -> IV c -> a -> Encrypted b e a
  decrypt :: (BlockCipher c) => c -> IV c -> Encrypted b e a -> Either e a

-- | Utility helper class represents idea of
-- 'BlockCipher' and 'IV' (initial vector)
-- hidden inside __m__ which often is
-- some sort of "application" monad which implements
-- 'Encryptor' class. Promotes finally tagless style.
class Encryptor m where
  encryptM :: (Encryptable b e a) => a -> m (Encrypted b e a)
  decryptM :: (Encryptable b e a) => Encrypted b e a -> m (Either e a)

instance Encryptable ByteString e ByteString where
  encrypt c i = Encrypted . ctrCombine c i
  decrypt c i = Right . ctrCombine c i . coerce

instance Encryptable ByteString e BL.ByteString where
  encrypt c i = reType . encrypt c i . BL.toStrict
  decrypt c i = second BL.fromStrict . decrypt c i . reType

instance Encryptable ByteString UnicodeException Text where
  encrypt c i = reType . encrypt c i . TE.encodeUtf8
  decrypt c i x = decrypt c i (reType x) >>= TE.decodeUtf8'

instance Encryptable ByteString UnicodeException TL.Text where
  encrypt c i = reType . encrypt c i . TL.toStrict
  decrypt c i = second TL.fromStrict . decrypt c i . reType

instance (Traversable f, Encryptable b e a) => Encryptable (f b) e (f a) where
  encrypt c i =
    Encrypted . ((coerce . (encrypt c i :: a -> Encrypted b e a) :: a -> b) <$>)
  decrypt c i xs =
    mapM (decrypt c i . Encrypted) (coerce xs :: f b)

-- | Casts original phantom type __a__ of 'Encrypted'
-- value to some other type __c__. Useful for building
-- 'Encryptable' instances on top of other already
-- existing 'Encryptable' instances.
reType :: Encrypted b e a -> Encrypted b e c
reType = Encrypted . coerce
