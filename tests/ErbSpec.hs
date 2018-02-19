module ErbSpec(spec) where

import           XPrelude

import           Test.Hspec

import           Erb

tests :: [(String, [RubyStatement])]
tests =
  [ ("port = 5432", [ Puts (Value (Literal "port = 5432" ))])
  , ("mode = host=<% @var %>", [ Puts (Value (Literal "mode = host=")), Puts (Object (Value (Literal "var"))), Puts (Value (Literal ""))])
  ]

alltests =
  for_ tests $ \(s, e) ->
    let item = it ("should parse " <> s)
    in
    case parseErbString s of
      Left err -> item $ expectationFailure (show err)
      Right r -> item $ r `shouldBe` e

spec = describe "Erb" $ alltests
