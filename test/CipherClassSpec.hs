module CipherClassSpec
  ( spec,
  )
where

import CipherClass
import Test.Hspec
import Universum

data Env = Env {cipher :: AES256, iv :: IV AES256}

spec :: Spec
spec = before newEnv $ do
  it "encrypt and decrypt ByteString successfully" $ \env -> do
    let encrypted :: Encrypted ByteString ByteString UnicodeException = encrypt (cipher env) (iv env) ("test" :: ByteString)
    let decrypted :: Either UnicodeException ByteString = decrypt (cipher env) (iv env) encrypted
    decrypted `shouldBe` Right ("test" :: ByteString)
  it "encrypt and decrypt Text successfully" $ \env -> do
    let encrypted :: Encrypted Text ByteString UnicodeException = encrypt (cipher env) (iv env) ("test" :: Text)
    let decrypted :: Either UnicodeException Text = decrypt (cipher env) (iv env) encrypted
    decrypted `shouldBe` Right ("test" :: Text)

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
