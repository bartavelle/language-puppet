module InterpreterSpec (spec, main) where

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
import           Puppet.Lens
import           Puppet.Parser
import           Puppet.Parser.Types

appendArrowNode :: Text
appendArrowNode = "appendArrow"

appendArrowInput :: Text
appendArrowInput = T.unlines [ "node " <> appendArrowNode <> " {"
                  , "user { 'jenkins':"
                  , "  groups => 'ci'"
                  , "}"
                  , "User <| title == 'jenkins' |> { groups +> 'docker'}"
                  , "}"
                  ]

spec :: Spec
spec = do
  describe "AppendArrow in AttributeDecl" $
    it "should append the group attribute in the user resource" $ do
      pendingWith "see issue #134"
      pureCompute appendArrowNode appendArrowInput ^.._1._Right._1.traverse.rattributes.at "groups"._Just._PArray.traverse._PString
        `shouldBe` ["ci", "docker"]

main :: IO ()
main = hspec spec

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
