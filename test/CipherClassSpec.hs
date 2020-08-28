{-# LANGUAGE FlexibleContexts #-}

module CipherClassSpec
  ( spec,
  )
where

import CipherClass
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Universum

data Env = Env {cipher :: AES256, iv :: IV AES256}

spec :: Spec
spec = before newEnv $ do
  it "ByteString" $ \env -> property $ \x ->
    reCrypt (cipher env) (iv env) x
      `shouldBe` (Right x :: Either () ByteString)
  it "BL.ByteString" $ \env -> property $ \x ->
    reCrypt (cipher env) (iv env) x
      `shouldBe` (Right x :: Either () BL.ByteString)
  it "Text" $ \env -> property $ \x ->
    reCrypt (cipher env) (iv env) x
      `shouldBe` (Right x :: Either UnicodeException Text)
  it "TL.Text" $ \env -> property $ \x ->
    reCrypt (cipher env) (iv env) x
      `shouldBe` (Right x :: Either UnicodeException TL.Text)
  where
    reCrypt ::
      forall c a e.
      (BlockCipher c, Encryptable a ByteString e) =>
      c ->
      IV c ->
      a ->
      Either e a
    reCrypt c i x = decrypt c i (encrypt c i x :: Encrypted a ByteString e)

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
