    {-# LANGUAGE OverloadedLists #-}
module Function.EachSpec (spec, main) where

import           Test.Hspec
import           Helpers

import           Puppet.Interpreter.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "should be callable as" $ do
        it "each on an array selecting each value" $ do
            c <- getCatalog "$a = [1,2,3]\n $a.each |$v| {\n file { \"/file_$v\": ensure => present } \n } "
            getResource (RIdentifier "file" "/file_1") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_2") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_3") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
        it "each on an array selecting each value - function call style" $ do
            c <- getCatalog "$a = [1,2,3]\n each ($a) |$index, $v| {\n file { \"/file_$v\": ensure => present }\n }"
            getResource (RIdentifier "file" "/file_1") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_2") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_3") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
        it "each on an array with index" $ do
            c <- getCatalog "$a = [present, absent, present]\n $a.each |$k,$v| {\n file { \"/file_$k\": ensure => $v }\n }"
            getResource (RIdentifier "file" "/file_0") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_1") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "absent"
            getResource (RIdentifier "file" "/file_2") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
        it "each on a hash selecting entries" $ do
            c <- getCatalog "$a = {'a'=>'present','b'=>'absent','c'=>'present'}\n $a.each |$e| {\n $num = $e[0]\n file { \"/file_${num}\": ensure => $e[1] }\n }"
            getResource (RIdentifier "file" "/file_a") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_b") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "absent"
            getResource (RIdentifier "file" "/file_c") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
        it "each on a hash selecting key and value" $ do
            c <- getCatalog "$a = {'a'=>present,'b'=>absent,'c'=>present}\n $a.each |$k, $v| {\n file { \"/file_$k\": ensure => $v }\n }"
            getResource (RIdentifier "file" "/file_a") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_b") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "absent"
            getResource (RIdentifier "file" "/file_c") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
        it "each on a hash selecting key and value (using captures-last parameter)" $ do
            pending
            c <- getCatalog "$a = {'a'=>present,'b'=>absent,'c'=>present}\n $a.each |*$kv| {\n file { \"/file_${kv[0]}\": ensure => $kv[1] }\n }"
            getResource (RIdentifier "file" "/file_a") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
            getResource (RIdentifier "file" "/file_b") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "absent"
            getResource (RIdentifier "file" "/file_c") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
    describe "should produce receiver" $
        it "each checking produced value using single expression" $ do
            pending
            c <- getCatalog "$a = [1, 3, 2]\n $b = $a.each |$x| { \"unwanted\" }\n $u = $b[1]\n file { \"/file_${u}\":\n ensure => present\n }"
            getResource (RIdentifier "file" "/file_3") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"


