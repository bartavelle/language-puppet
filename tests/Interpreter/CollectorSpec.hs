{-# LANGUAGE OverloadedLists #-}
-- | Directly ported from puppet specs
module Interpreter.CollectorSpec (spec) where

import           Test.Hspec

import           Control.Lens
import qualified Data.Text as Text

import           Helpers


shouldNotify :: [Text] -> [PValue] -> Expectation
shouldNotify content expectedMessages = do
    catalog <- case pureCatalog (Text.unlines content) of
      Left rr -> fail rr
      Right x -> return x
    let messages = itoList catalog ^.. folded . filtered (\rp -> rp ^. _1 . itype == "notify") . _2 . rattributes . ix "message"
    messages `shouldMatchList` expectedMessages

shouldFail :: [Text] -> Expectation
shouldFail content = let catalog :: Either String FinalCatalog
                         catalog = pureCatalog (Text.unlines content)
                     in  catalog `shouldSatisfy` has _Left

spec =
  describe "Collectors" $ do
    spec0
    spec1

spec0 :: Spec
spec0 = do
    it "matches everything when no query given" $
        [ "@notify { 'testing': message => 'the message' }"
        , "@notify { 'other': message => 'the other message' }"
        , "Notify <| |>"
        ] `shouldNotify` ["the other message", "the message"]
    it "matches regular resources" $
        [ "notify { \"testing\": message => \"the message\" }"
        , "notify { \"other\": message => \"the other message\" }"
        , "Notify <| |> { message => \"changed\" }"
        ] `shouldNotify` ["changed", "changed"]
    it "matches on tags" $
        [ "@notify { \"testing\": tag => [\"one\"], message => \"wanted\" }"
        , "@notify { \"other\": tag => [\"two\"], message => \"unwanted\" }"
        , "Notify <| tag == one |>"
        ] `shouldNotify` ["wanted"]
    it "matches on title" $
        [ "@notify { \"testing\": message => \"the message\" }"
        , "Notify <| title == \"testing\" |>"
        ] `shouldNotify` ["the message"]
    it "matches on other parameters" $
        [ "@notify { \"testing\": message => \"the message\" }"
        , "@notify { \"other testing\": message => \"the wrong message\" }"
        , "Notify <| message == \"the message\" |>"
        ] `shouldNotify` ["the message"]
    it "matches against elements of an array valued parameter" $
        [ "@notify { \"testing\": message => [\"the\", \"message\"] }"
        , "@notify { \"other testing\": message => [\"not\", \"here\"] }"
        , "Notify <| message == \"message\" |>"
        ] `shouldNotify` [PArray ["the", "message"]]
    it "matches with bare word" $
        [ "@notify { \"testing\": tag => [\"one\"], message => \"wanted\" }"
        , "Notify <| tag == one |>"
        ] `shouldNotify` ["wanted"]
    it "matches with single quoted string" $
        [ "@notify { \"testing\": tag => [\"one\"], message => \"wanted\" }"
        , "Notify <| tag == 'one' |>"
        ] `shouldNotify` ["wanted"]
    it "matches with double quoted string" $
        [ "@notify { \"testing\": tag => [\"one\"], message => \"wanted\" }"
        , "Notify <| tag == \"one\" |>"
        ] `shouldNotify` ["wanted"]
    it "matches with double quoted string with interpolated expression" $
        [ "@notify { \"testing\": tag => [\"one\"], message => \"wanted\" }"
        , "$x = 'one'"
        , "Notify <| tag == \"$x\" |>"
        ] `shouldNotify` ["wanted"]
    it "matches with resource references" $ do
        pending
        shouldNotify [ "@notify { \"foobar\": }"
                     , "@notify { \"testing\": require => Notify[\"foobar\"], message => \"wanted\" }"
                     , "Notify <| require == Notify[\"foobar\"] |>"
                     ]
                     ["wanted"]
    it "allows criteria to be combined with 'and'" $
        [ "@notify { \"testing\": message => \"the message\" }"
        , "@notify { \"other\": message => \"the message\" }"
        , "Notify <| title == \"testing\" and message == \"the message\" |>"
        ] `shouldNotify` ["the message"]
    it "allows criteria to be combined with 'or'" $
        [ "@notify { \"testing\": message => \"the message\" }"
        , "@notify { \"other\": message => \"other message\" }"
        , "@notify { \"yet another\": message => \"different message\" }"
        , "Notify <| title == \"testing\" or message == \"other message\" |>"
        ] `shouldNotify` ["the message", "other message"]
    it "allows criteria to be grouped with parens" $
        [ "@notify { \"testing\":     message => \"different message\", withpath => true }"
        , "@notify { \"other\":       message => \"the message\" }"
        , "@notify { \"yet another\": message => \"the message\",       withpath => true }"
        , "Notify <| (title == \"testing\" or message == \"the message\") and withpath == true |>"
        ] `shouldNotify` ["the message", "different message"]
    it "does not do anything if nothing matches" $
        [ "@notify { \"testing\": message => \"different message\" }"
        , "Notify <| title == \"does not exist\" |>"
        ] `shouldNotify` []
    it "excludes items with inequalities" $
        [ "@notify { \"testing\": message => \"good message\" }"
        , "@notify { \"the wrong one\": message => \"bad message\" }"
        , "Notify <| title != \"the wrong one\" |>"
        ] `shouldNotify` ["good message"]
    it "does not exclude resources with unequal arrays" $
        [ "@notify { \"testing\": message => \"message\" }"
        , "@notify { \"the wrong one\": message => [\"not this message\", \"or this one\"] }"
        , "Notify <| message != \"not this message\" |>"
        ] `shouldNotify` ["message", PArray ["not this message", "or this one"]]
    it "does not exclude tags with inequalities" $
        [ "@notify { \"testing\": tag => [\"wanted\"], message => \"wanted message\" }"
        , "@notify { \"other\": tag => [\"why\"], message => \"the way it works\" }"
        , "Notify <| tag != \"why\" |>"
        ] `shouldNotify` ["wanted message", "the way it works"]
    it "does not collect classes" $ shouldFail $
        [ "class theclass {"
        , "    @notify { \"testing\": message => \"good message\" }"
        , "}"
        , "Class <|  |>"
        ]
    it "does not collect resources that don't exist" $ do
        pending
        shouldFail $
            [ "class theclass {"
            , "    @notify { \"testing\": message => \"good message\" }"
            , "}"
            , "SomeResource <|  |>"
            ]
    it "modifies an existing array" $
        [ "@notify { \"testing\": message => [\"original message\"] }"
        , "Notify <| |> {"
        , "    message +> \"extra message\""
        , "}"] `shouldNotify` [PArray ["original message", "extra message"]]
    it "converts a scalar to an array" $
        [ "@notify { \"testing\": message => \"original message\" }"
        , "Notify <| |> {"
        , "    message +> \"extra message\""
        , "}"] `shouldNotify` [PArray ["original message", "extra message"]]
    it "collects and overrides virtual resources multiple times using multiple collects" $
        [ "@notify { \"testing\": message => \"original\" }"
        , "Notify <|  |> { message => 'overridden1' }"
        , "Notify <|  |> { message => 'overridden2' }"
        ] `shouldNotify` ["overridden2"]
    it "collects and overrides non virtual resources multiple times using multiple collects" $
        [ "notify { \"testing\": message => \"original\" }"
        , "Notify <|  |> { message => 'overridden1' }"
        , "Notify <|  |> { message => 'overridden2' }"
        ] `shouldNotify` ["overridden2"]


spec1 :: Spec
spec1 = do
  let computeWith = pureCatalog . arrowOperationInput
  describe "Resource Collector" $
    it "should append the new 'uid' attribute in the user resource" $
      getResAttr (computeWith "=>") ^. at "uid" `shouldBe` Just (PNumber 1000)
  describe "AppendArrow in AttributeDecl" $
    it "should add 'docker' to the 'groups' attribute of the user resource" $ do
      getResAttr (computeWith "+>") ^. at "groups" `shouldBe` Just (PArray $ ["ci", "docker"])
  describe "AssignArrow in AttributeDecl" $
    it "should override the 'groups' attributes from the user resource" $
      getResAttr (computeWith "=>") ^. at "groups" `shouldBe` Just (PArray $ ["docker"])
  where
    getResAttr :: (Either String FinalCatalog) -> Container PValue
    getResAttr s = s ^. _Right . at (RIdentifier "user" "jenkins")._Just.rattributes

    arrowOperationInput :: Text -> Text
    arrowOperationInput arr =
      Text.unlines [ "user { 'jenkins':"
                   , "  groups => 'ci'"
                   , "}"
                   , "User <| title == 'jenkins' |> {"
                   , "groups " <> arr <> " 'docker',"
                   , "uid => 1000}"
                   , "}"
                   ]
