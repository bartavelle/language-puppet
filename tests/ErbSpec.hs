module ErbSpec(spec) where

import           XPrelude

import           Test.Hspec

import           Erb

tests :: [(String, [RubyStatement])]
tests =
  [ ("port = 5432", [ Puts (Value (Literal "port = 5432" ))])
  , ("mode = host=<% @var %>", [ Puts (Value (Literal "mode = host="))
                               , Puts (Object (Value (Literal "var")))
                               , Puts (Value (Literal ""))])
  , ("<%= @repuser['name'] %>", [ Puts (Value (Literal ""))
                                , Puts (LookupOperation (Object (Value (Literal "repuser"))) (Value (Literal "name")))
                                , Puts (Value (Literal ""))])
  -- , ("<% if false %>ko<% end -%>", [ Puts (Value (Literal ""))
  --                                 , Puts (LookupOperation (Object (Value (Literal "repuser"))) (Value (Literal "name")))
  --                                 , Puts (Value (Literal ""))])
  ]

bogus= [ "<% var %>"]

positivetests =
  for_ tests $ \(s, e) ->
    let item = it ("should parse " <> s)
    in
    case parseErbString s of
      Left err -> item $ expectationFailure (show err)
      Right r -> item $ r `shouldBe` e

negativetests =
  for_ bogus $ \b ->
    it "should fail to parse" $ do
      pendingWith "In puppet 4, a variable need its @ prefix. See issue #237"
      (parseErbString b) `shouldSatisfy` isLeft

spec = describe "Erb" $ do
  positivetests
  negativetests
