module PuppetDB.Dummy where

import Puppet.Interpreter.Types
import qualified Data.Either.Strict as S

dummyPuppetDB :: PuppetDBAPI
dummyPuppetDB = PuppetDBAPI
                    (const (return (S.Left "not implemented")))
                    (const (return (S.Left "not implemented")))
                    (const (return (S.Left "not implemented")))
                    (const (return (S.Left "not implemented")))
                    (const (return (S.Left "not implemented")))
                    (return (S.Left "not implemented"))

