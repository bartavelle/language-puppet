module InterpreterSpec (collectorSpec, classIncludeSpec, main) where

import           Control.Lens
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Test.Hspec
import           Text.Megaparsec          (eof, parse)

import           Puppet.Interpreter
import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types
-- import           Puppet.Lens
import           Puppet.Parser
import           Puppet.Parser.Types
import           Puppet.PP

appendArrowNode :: Text
appendArrowNode = "appendArrow"

arrowOperationInput :: ArrowOp -> Text
arrowOperationInput arr = T.unlines [ "node " <> appendArrowNode <> " {"
                  , "user { 'jenkins':"
                  , "  groups => 'ci'"
                  , "}"
                  , "User <| title == 'jenkins' |> {"
                  , "groups " <> (prettyToText . pretty) arr <> " 'docker',"
                  , "uid => 1000}"
                  , "}"
                  ]

getResAttr ::
  (Either
     PrettyError
     (FinalCatalog, EdgeMap, FinalCatalog, [Resource]),
      InterpreterState,
      InterpreterWriter)
  -> Container PValue
getResAttr s =
  let finalcatalog = s ^._1._Right._1
  in finalcatalog ^. at (RIdentifier "user" "jenkins")._Just.rattributes


collectorSpec :: Spec
collectorSpec = do
  let computeWith arr = pureCompute appendArrowNode (arrowOperationInput arr)
  describe "Resource Collector" $
    it "should append the new 'uid' attribute in the user resource" $
      getResAttr (computeWith AssignArrow) ^. at "uid" `shouldBe` Just (PNumber 1000)
  describe "AppendArrow in AttributeDecl" $
    it "should add 'docker' to the 'groups' attribute of the user resource" $
      getResAttr (computeWith AppendArrow) ^. at "groups" `shouldBe` Just (PArray $ V.fromList ["ci", "docker"])
  describe "AssignArrow in AttributeDecl" $
    it "should override the 'groups' attributes from the user resource" $
      getResAttr (computeWith AssignArrow) ^. at "groups" `shouldBe` Just (PArray $ V.fromList ["docker"])

classIncludeSpec :: Spec
classIncludeSpec = do
    let compute i = pureCompute "dummy" i ^. _1
    describe "Multiple loading" $ do
        it "should work when using several include statements" $ compute (T.unlines [ "node 'dummy' {",  "include foo",  "include foo", "}" ]) `shouldSatisfy` (has _Right)
        it "should work when using class before include" $ compute (T.unlines [ "node 'dummy' {",  "class { 'foo': }",  "include foo", "}" ]) `shouldSatisfy` (has _Right)
        it "should work when using include before class" $ compute (T.unlines [ "node 'dummy' {",  "include foo", "class { 'foo': }", "}" ]) `shouldSatisfy` (has _Right)

main :: IO ()
main = hspec $ do
    describe "Collectors" collectorSpec
    describe "Class inclusion" classIncludeSpec

-- | Given a node and raw text input to be parsed, compute the manifest in a dummy setting.
pureCompute :: NodeName
            -> Text
            -> (Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]),
                InterpreterState,
                InterpreterWriter)
pureCompute node input =
  let hush :: Show a => Either a b -> b
      hush = either (error . show) id

      getStatement :: NodeName -> Text -> HashMap (TopLevelType, NodeName) Statement
      getStatement n i = HM.fromList [ ((TopNode, n), nodeStatement i)
                                     , ((TopClass, "foo"), ClassDeclaration $ ClassDecl mempty mempty mempty mempty (initialPPos "dummy"))
                                     ]

      nodeStatement :: Text -> Statement
      nodeStatement i = V.head $ hush $ parse (puppetParser <* eof) "test" i

  in pureEval dummyFacts (getStatement node input) (computeCatalog node)
