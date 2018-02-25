module ErbSpec(spec) where

import           XPrelude

import           Test.Hspec

import           Erb
import           Puppet.Runner

parsingtests :: [(String, [RubyStatement])]
parsingtests =
  [ ("port = 5432", [ Puts (Value (Literal "port = 5432" ))])
  , ("mode = host=<% @var %>", [ Puts (Value (Literal "mode = host="))
                               , Puts (Object (Value (Literal "var")))
                               , Puts (Value (Literal ""))])
  , ("<%= @repuser['name'] %>", [ Puts (Value (Literal ""))
                                , Puts (LookupOperation (Object (Value (Literal "repuser"))) (Value (Literal "name")))
                                , Puts (Value (Literal ""))])
  ]

resolvetests :: [([RubyStatement], Text)]
resolvetests =
  [ ([ Puts (Object (Value (Literal "hostname")))], "dummy")
  , ([ Puts (LookupOperation (Object (Value (Literal "os"))) (Value (Literal "architecture")))], "amd64")
  ]

parsingspec =
  for_ parsingtests $ \(s, e) ->
    let item = it ("should parse " <> s) in
    case parseErbString s of
      Left err -> item $ expectationFailure (show err)
      Right r -> item $ r `shouldBe` e

resolvespec =
  let state0 = initialState dummyFacts mempty
      Just (scope_name, scope) = extractFromState state0
  in
  for_ resolvetests $ \(s, e) ->
    let item = it ("should resolve " <> show s) in
    case rubyEvaluate scope scope_name s of
      Left err -> item $ do {pendingWith "See #213"; expectationFailure (show err)}
      Right r -> item $ r `shouldBe` e

spec = describe "Erb" $ do
  parsingspec
  resolvespec
