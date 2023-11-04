module ErbSpec (spec) where

import Erb
import GHC.Base (error)
import Puppet.Runner
import Test.Hspec
import XPrelude

parsingtests :: [(String, [RubyStatement])]
parsingtests =
  [ ("port = 5432", [Puts (Value (Literal "port = 5432"))]),
    ( "mode = host=<%= @var %>",
      [ Puts (Value (Literal "mode = host=")),
        Puts (Object (Value (Literal "@var"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "mode = host=<%= var %>",
      [ Puts (Value (Literal "mode = host=")),
        Puts (Object (Value (Literal "var"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "<%= @os['architecture'] %>",
      [ Puts (Value (Literal "")),
        Puts (LookupOperation (Object (Value (Literal "@os"))) (Value (Literal "architecture"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "<%= @os['release']['major'] %>",
      [ Puts (Value (Literal "")),
        Puts (LookupOperation (LookupOperation (Object (Value (Literal "@os"))) (Value (Literal "release"))) (Value (Literal "major"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "<%= @processors['models'] %>",
      [ Puts (Value (Literal "")),
        Puts (LookupOperation (Object (Value (Literal "@processors"))) (Value (Literal "models"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "<%= scope.lookupvar('::fqdn') %>",
      [ Puts (Value (Literal "")),
        Puts (ScopeObject (Value (Literal "::fqdn"))),
        Puts (Value (Literal ""))
      ]
    ),
    ( "<%= scope.lookupvar(\"::fqdn\") %>",
      [ Puts (Value (Literal "")),
        Puts (ScopeObject (Value (Literal "::fqdn"))),
        Puts (Value (Literal ""))
      ]
    )
  ]

resolvetests :: [([RubyStatement], Text)]
resolvetests =
  [ ( [Puts (Object (Value (Literal "@hostname")))],
      "dummy"
    ),
    ( [Puts (LookupOperation (Object (Value (Literal "@os"))) (Value (Literal "architecture")))],
      "amd64"
    ),
    ( [Puts (LookupOperation (LookupOperation (Object (Value (Literal "@os"))) (Value (Literal "release"))) (Value (Literal "major")))],
      "7"
    ),
    ( [Puts (LookupOperation (Object (Value (Literal "@processors"))) (Value (Literal "models")))],
      expectedmodels
    ),
    ( [Puts (ScopeObject (Value (Literal "::fqdn")))],
      "dummy.dummy.domain"
    )
  ]
  where
    expectedmodels = "[\"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\", \"Intel(R) Core(TM) i7 CPU         860  @ 2.80GHz\"]"

parsingspec =
  for_ parsingtests $ \(s, e) ->
    let item = it ("should parse " <> s)
     in case parseErbString s of
          Left err -> item $ expectationFailure (show err)
          Right r -> item $ r `shouldBe` e

resolvespec =
  let state0 = initialState dummyFacts mempty
      (scope_name, scope) = case extractScope state0 of
        Just p -> p
        Nothing -> error "should not happen"
   in for_ resolvetests $ \(s, e) ->
        let item = it ("should resolve " <> show s)
         in case rubyEvaluate scope scope_name s of
              Left err -> item $ expectationFailure (show err)
              Right r -> item $ r `shouldBe` e

spec = describe "Erb" $ do
  parsingspec
  resolvespec
