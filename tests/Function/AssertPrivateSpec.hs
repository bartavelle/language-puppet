{-# LANGUAGE OverloadedLists #-}
module Function.AssertPrivateSpec where

import           Helpers

import           Puppet.Interpreter (initialState)


main :: IO ()
main = hspec spec

evalWithScope :: ([PValue] -> InterpreterMonad PValue)
              -> Text -- ^ caller scope
              -> Text -- ^ module scope
              -> [Expression]         -- ^ function args
              -> Either String PValue
evalWithScope apFunc callerScope moduleScope = (_Left %~ show) . view _1 . ctxEval . (mapM resolveExpression >=> apFunc)
    where
        ctxEval = pureEval' mempty state0
        state0 = initialState dummyFacts [("confdir", "/etc/puppet")] & curScope .~ [ContClass moduleScope, ContClass callerScope]


spec :: Spec
spec = withStdlibFunction "assert_private" $ \apFunc -> do
    let errorWith a b = case a of
                            Right x -> fail ("Should have failed, got this instead: " ++ show x)
                            Left rr -> rr `shouldContain` b
    it "should work when called from inside module" (evalWithScope apFunc "bar" "bar" [] `shouldBe` Right PUndef)
    it "should fail with the default message" (evalWithScope apFunc "bar" "baz" [] `errorWith` "is private")
    it "should fail with an explicit failure message" (evalWithScope apFunc "bar" "baz" ["lalala"] `errorWith` "lalala")
