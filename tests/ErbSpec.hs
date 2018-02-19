module ErbSpec(spec) where

import           XPrelude

import           Test.Hspec

import           Erb

tests :: [(FilePath, [RubyStatement])]
tests =
  [ ("simple.erb", [ Puts (Value (Literal "port = 5432\n" ))])
  , ("var.erb", [ Puts (Value (Literal "mode = host=")), Puts (Object (Value (Literal "var"))), Puts (Value (Literal "\n"))])
  ]

alltests =
  for_ tests $ \(fp, e) ->
    test ("tests/erb/" <> fp) e
  where
    test fp expected = do
      let item = it ("should parse " <> fp)
      runIO (parseErbFile fp) >>= \case
        Left err -> item $ expectationFailure (show err)
        Right r -> item $ r `shouldBe` expected

spec = describe "Erb" $ alltests
