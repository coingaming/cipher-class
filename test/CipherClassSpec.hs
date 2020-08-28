module CipherClassSpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TestUtil
import Universum

spec :: Spec
spec = before newEnv $ do
  --
  -- TODO : Traversable and Persistent properties
  --
  it "ByteString/ByteString" $ \env -> property $ \x ->
    reCryptBS env x
      `shouldBe` (Right x :: Either () ByteString)
  it "BL.ByteString/ByteString" $ \env -> property $ \x ->
    reCryptBS env x
      `shouldBe` (Right x :: Either () BL.ByteString)
  it "Text/ByteString" $ \env -> property $ \x ->
    reCryptBS env x
      `shouldBe` (Right x :: Either UnicodeException Text)
  it "TL.Text/ByteString" $ \env -> property $ \x ->
    reCryptBS env x
      `shouldBe` (Right x :: Either UnicodeException TL.Text)
