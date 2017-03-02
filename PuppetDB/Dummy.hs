-- | A dummy implementation of 'PuppetDBAPI', that will return empty
-- responses.
module PuppetDB.Dummy where

import Puppet.Interpreter.Types
import Control.Monad.Except

dummyPuppetDB :: Monad m => PuppetDBAPI m
dummyPuppetDB = PuppetDBAPI
                    (return "dummy")
                    (const (return ()))
                    (const (return ()))
                    (const (return ()))
                    (const (throwError "not implemented"))
                    (const (return [] ))
                    (const (return [] ))
                    (throwError "not implemented")
                    (\_ _ -> return [] )
