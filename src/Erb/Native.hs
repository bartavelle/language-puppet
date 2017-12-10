module Erb.Native where

import Puppet.Interpreter.Type
import Foreign.Ruby

instance FromRuby ResolvedValue where
    fromRuby v = fromRuby v >>= \case
            Just h -> return (fromJSON h)
            Nothing -> return Nothing

instance ToRuby ResolvedValue where
    toRuby = toRuby . toJSON
