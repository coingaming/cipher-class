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
newtype Encrypted a b e = Encrypted b
  deriving (PersistField, PersistFieldSql)

-- | Class represents the idea of
-- typed symmetric encryption and decryption
class Encryptable a b e where
  encrypt :: (BlockCipher c) => c -> IV c -> a -> Encrypted a b e
  decrypt :: (BlockCipher c) => c -> IV c -> Encrypted a b e -> Either e a

-- | Utility helper class represents idea of
-- 'BlockCipher' and 'IV' (initial vector)
-- hidden inside __m__ which often is
-- some sort of "application" monad which implements
-- 'Encryptor' class. Promotes finally tagless style.
class Encryptor m where
  encryptM :: (Encryptable a b e) => a -> m (Encrypted a b e)
  decryptM :: (Encryptable a b e) => Encrypted a b e -> m (Either e a)

instance Encryptable ByteString ByteString e where
  encrypt c i = Encrypted . ctrCombine c i
  decrypt c i = Right . ctrCombine c i . coerce

instance Encryptable BL.ByteString ByteString e where
  encrypt c i = reType . encrypt c i . BL.toStrict
  decrypt c i = second BL.fromStrict . decrypt c i . reType

instance Encryptable Text ByteString UnicodeException where
  encrypt c i = reType . encrypt c i . TE.encodeUtf8
  decrypt c i x = decrypt c i (reType x) >>= TE.decodeUtf8'

instance Encryptable TL.Text ByteString UnicodeException where
  encrypt c i = reType . encrypt c i . TL.toStrict
  decrypt c i = second TL.fromStrict . decrypt c i . reType

instance (Traversable f, Encryptable a b e) => Encryptable (f a) (f b) e where
  encrypt c i =
    Encrypted . ((coerce . (encrypt c i :: a -> Encrypted a b e) :: a -> b) <$>)
  decrypt c i xs =
    mapM (decrypt c i . Encrypted) (coerce xs :: f b)

-- | Casts original phantom type __a__ of 'Encrypted'
-- value to some other type __c__. Useful for building
-- 'Encryptable' instances on top of other already
-- existing 'Encryptable' instances.
reType :: Encrypted a b e -> Encrypted c b e
reType = Encrypted . coerce
