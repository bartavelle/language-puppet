-- | A dummy implementation of 'PuppetDBAPI', that will return empty
-- responses.
module PuppetDB.Dummy where

import Puppet.Interpreter.Types
import Control.Monad.Trans.Either

dummyPuppetDB :: Monad m => PuppetDBAPI m
dummyPuppetDB = PuppetDBAPI
                    (return "dummy")
                    (const (return ()))
                    (const (return ()))
                    (const (return ()))
                    (const (left "not implemented"))
                    (const (return [] ))
                    (const (return [] ))
                    (left "not implemented")
                    (\_ _ -> return [] )

