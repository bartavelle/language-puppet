{-# LANGUAGE OverloadedLists #-}
module Function.AssertPrivateSpec where

import           Test.Hspec

import           Control.Lens
import           Control.Monad
import           Data.Text (Text)

import           Puppet.Interpreter.Resolve
import           Puppet.Interpreter.Pure
import           Puppet.Interpreter.Types
import           Puppet.Interpreter.Utils (initialState)
import           Puppet.Interpreter.IO (interpretMonad)
import           Puppet.Stdlib
import           Puppet.Parser.Types

main :: IO ()
main = hspec spec

evalWithScope :: ([PValue] -> InterpreterMonad PValue)
              -> Text -- ^ caller scope
              -> Text -- ^ module scope
              -> [Expression]         -- ^ function args
              -> Either String PValue
evalWithScope apFunc callerScope moduleScope = (_Left %~ show) . view _1 . ctxEval . (mapM resolveExpression >=> apFunc)
    where
        ctxEval = runIdentity . interpretMonad (pureReader mempty) startingState
        startingState = initialState dummyFacts [("confdir", "/etc/puppet")] & curScope .~ [ContClass moduleScope, ContClass callerScope]


spec :: Spec
spec = do
    apFunc <- case stdlibFunctions ^? ix "assert_private" of
                  Just f -> return f
                  Nothing -> fail "Don't know the size function"
    let errorWith a b = case a of
                            Right x -> fail ("Should have failed, got this instead: " ++ show x)
                            Left rr -> rr `shouldContain` b
    it "should work when called from inside module" (evalWithScope apFunc "bar" "bar" [] `shouldBe` Right PUndef)
    it "should fail with the default message" (evalWithScope apFunc "bar" "baz" [] `errorWith` "is private")
    it "should fail with an explicit failure message" (evalWithScope apFunc "bar" "baz" ["lalala"] `errorWith` "lalala")
