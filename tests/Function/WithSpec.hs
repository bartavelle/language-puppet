{-# LANGUAGE OverloadedLists #-}
module Function.WithSpec where

import           Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let mgetCatalog x = case getCatalog x of
                        Left rr -> fail rr
                        Right y -> return y
  describe "should run as" $ do
    it "should be callable with an argument" $
      mgetCatalog "with ( 12 ) |$x| { file {'/f': content => $x; } }"
       >>= getResource (RIdentifier "file" "/f")
       >>= getAttribute "content"
       >>= (`shouldBe` "12")
    it "should be callable with two arguments" $
      mgetCatalog "with ( '/tmp/lal', 12 ) |$f, $x| { file {$f: content => $x; } }"
       >>= getResource (RIdentifier "file" "/tmp/lal")
       >>= getAttribute "content"
       >>= (`shouldBe` "12")
    it "should override scope" $
      mgetCatalog "$x='lol' \n with ( 12 ) |$x| { file {'/f': content => $x; } }"
       >>= getResource (RIdentifier "file" "/f")
       >>= getAttribute "content"
       >>= (`shouldBe` "12")


