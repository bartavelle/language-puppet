-- | A dummy implementation of 'PuppetDBAPI', that will return empty
-- responses.
module PuppetDB.Dummy where

import Puppet.Interpreter.Types
import qualified Data.Either.Strict as S

dummyPuppetDB :: Monad m => PuppetDBAPI m
dummyPuppetDB = PuppetDBAPI
                    (return "dummy")
                    (const (return (S.Right () )))
                    (const (return (S.Right () )))
                    (const (return (S.Right () )))
                    (const (return (S.Left "not implemented")))
                    (const (return (S.Right [] )))
                    (const (return (S.Right [] )))
                    (return (S.Left "not implemented"))
                    (\_ _ -> return (S.Right [] ))

