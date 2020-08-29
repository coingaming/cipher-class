{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestEnv
  ( Env (..),
    newEnv,
    reCryptBS,
    Login (..),
    Address (..),
  )
where

import CipherClass
import Data.Coerce (coerce)
import Database.Esqueleto (PersistField, PersistFieldSql)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Universum
import Prelude (Show (..))

data Env = Env {cipher :: AES256, iv :: IV AES256}

newEnv :: IO Env
newEnv = do
  c0 :: ByteString <- getRandomBytes 32
  i0 :: ByteString <- getRandomBytes 16
  c <- case cipherInit c0 of
    CryptoPassed x -> return x
    CryptoFailed _ -> fail "BAD_CIPHER"
  i <- case makeIV i0 of
    Just x -> return x
    Nothing -> fail "BAD_IV"
  return $ Env c i

reCryptBS ::
  forall a e.
  (Encryptable a ByteString e) =>
  Env ->
  a ->
  Either e a
reCryptBS env x =
  decrypt c i (encrypt c i x :: Encrypted a ByteString e)
  where
    c = cipher env
    i = iv env

newtype Login
  = Login Text
  deriving newtype (Eq, Arbitrary, Show, PersistField, PersistFieldSql)

newtype Address
  = Address Text
  deriving newtype (Eq, Arbitrary)

instance Show Address where
  show = const "SECRET"

instance Encryptable Address ByteString UnicodeException where
  encrypt c i x = reType $ encrypt c i (coerce x :: Text)
  decrypt c i = second Address . decrypt c i . reType
