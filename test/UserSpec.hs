{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UserSpec
  ( spec,
    -- just supressing warning
    UserStorageId,
  )
where

import CipherClass
import Data.Coerce (coerce)
import Database.Persist.TH
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()
import TestUtil
import Universum

data User = User {login :: Login, password :: Password}
  deriving (Eq, Generic, Show)

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

share
  [mkPersist sqlSettings]
  [persistLowerCase|
    UserStorage
      login Login
      password (Encrypted Password ByteString UnicodeException)
      UniqueUserStorage login
  |]

instance Encryptable User UserStorage UnicodeException where
  encrypt c i x = Encrypted $ UserStorage (login x) $ encrypt c i (password x)
  decrypt c i x0 = do
    let x = coerce x0
    pwd <- decrypt c i $ userStoragePassword x
    return $ User (userStorageLogin x) pwd

spec :: Spec
spec = before newEnv
  $ it "User/UserStorage"
  $ \env -> property $ \x -> do
    let c = cipher env
    let i = iv env
    decrypt c i (encrypt c i x :: Encrypted User UserStorage UnicodeException)
      `shouldBe` Right x
