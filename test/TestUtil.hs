{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestUtil
  ( Env (..),
    newEnv,
    reCryptBS,
    Login (..),
    Password (..),
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
  deriving newtype (PersistField, PersistFieldSql, Show, Eq, Arbitrary)

newtype Password
  = Password Text
  deriving newtype (PersistField, PersistFieldSql, Eq, Arbitrary)

instance Show Password where
  show = const "SECRET"

instance Encryptable Password ByteString UnicodeException where
  encrypt c i x = reType $ encrypt c i (coerce x :: Text)
  decrypt c i = second Password . decrypt c i . reType
