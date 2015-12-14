module InterpreterSpec (collectorSpec, main) where

import           Control.Lens
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromJust)
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
                  , "uid => 100}"
                  , "}"
                  ]

getResAttr ::
  (Either
     PrettyError
     (FinalCatalog, EdgeMap, FinalCatalog, [Resource]),
      InterpreterState,
      InterpreterWriter)
  -> Text
  -> Maybe PValue
getResAttr s n =
  let finalcatalog = s ^._1._Right._1
      res = fromJust $ finalcatalog ^. at (RIdentifier "user" "jenkins")
  in  res ^.rattributes.at n

collectorSpec :: Spec
collectorSpec = do
  let computeWith arr= pureCompute appendArrowNode (arrowOperationInput arr)
  describe "Resource Collector" $
    it "should append the new 'uid' attribute in the user resource" $ do
      pendingWith "see issue #165"
      getResAttr (computeWith AssignArrow) "uid" `shouldBe` Just (PNumber 100)
  describe "AppendArrow in AttributeDecl" $
    it "should add 'docker' to the 'groups' attribute of the user resource" $ do
      pendingWith "see issue #134"
      getResAttr (computeWith AppendArrow) "groups" `shouldBe` Just (PArray $ V.fromList ["docker", "ci"])
  describe "AssignArrow in AttributeDecl" $
    it "should first create a user with the group docker and then add 'ci' to its groups" $ do
      pendingWith "see issue #165"
      getResAttr (computeWith AssignArrow) "groups" `shouldBe` Just (PArray $ V.fromList ["docker", "ci"])

main :: IO ()
main = hspec collectorSpec

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
      getStatement n i = HM.singleton (TopNode, n) (nodeStatement i)
      nodeStatement :: Text -> Statement

      nodeStatement i = V.head $ hush $ parse (puppetParser <* eof) "test" i

  in pureEval dummyFacts (getStatement node input) (computeCatalog node)
