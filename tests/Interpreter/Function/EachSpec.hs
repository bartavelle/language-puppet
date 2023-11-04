{-# LANGUAGE OverloadedLists #-}

module Interpreter.Function.EachSpec (spec, main) where

import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let getCatalog x = case pureCatalog x of
        Left rr -> fail rr
        Right (y, _) -> pure y
  describe "should be callable as" $ do
    let checkEnsure f v c =
          getResource (RIdentifier "file" f) c >>= getAttribute "ensure" >>= \a -> a `shouldBe` v
        checks :: [(Text, PValue)] -> FinalCatalog -> IO ()
        checks lst c = mapM_ (\(f, v) -> checkEnsure f v c) lst
    it "each on an array selecting each value" $
      getCatalog "$a = [1,2,3]\n $a.each |$v| {\n file { \"/file_$v\": ensure => present } \n } "
        >>= checks
          [ ("/file_1", "present"),
            ("/file_2", "present"),
            ("/file_3", "present")
          ]
    it "each on an array selecting each value - function call style" $
      getCatalog "$a = [1,2,3]\n each ($a) |$index, $v| {\n file { \"/file_$v\": ensure => present }\n }"
        >>= checks
          [ ("/file_1", "present"),
            ("/file_2", "present"),
            ("/file_3", "present")
          ]
    it "each on an array with index" $
      getCatalog "$a = [present, absent, present]\n $a.each |$k,$v| {\n file { \"/file_$k\": ensure => $v }\n }"
        >>= checks
          [ ("/file_0", "present"),
            ("/file_1", "absent"),
            ("/file_2", "present")
          ]
    it "each on a hash selecting entries" $
      getCatalog "$a = {'a'=>'present','b'=>'absent','c'=>'present'}\n $a.each |$e| {\n $num = $e[0]\n file { \"/file_${num}\": ensure => $e[1] }\n }"
        >>= checks
          [ ("/file_a", "present"),
            ("/file_b", "absent"),
            ("/file_c", "present")
          ]
    it "each on a hash selecting key and value" $
      getCatalog "$a = {'a'=>present,'b'=>absent,'c'=>present}\n $a.each |$k, $v| {\n file { \"/file_$k\": ensure => $v }\n }"
        >>= checks
          [ ("/file_a", "present"),
            ("/file_b", "absent"),
            ("/file_c", "present")
          ]
    it "each on a hash selecting key and value (using captures-last parameter)" $ do
      pending
      getCatalog "$a = {'a'=>present,'b'=>absent,'c'=>present}\n $a.each |*$kv| {\n file { \"/file_${kv[0]}\": ensure => $kv[1] }\n }"
        >>= checks
          [ ("/file_a", "present"),
            ("/file_b", "absent"),
            ("/file_c", "present")
          ]
  describe "should produce receiver" $
    it "each checking produced value using single expression" $ do
      pending
      c <- getCatalog "$a = [1, 3, 2]\n $b = $a.each |$x| { \"unwanted\" }\n $u = $b[1]\n file { \"/file_${u}\":\n ensure => present\n }"
      getResource (RIdentifier "file" "/file_3") c >>= getAttribute "ensure" >>= \a -> a `shouldBe` "present"
