module Erb.Native where

import Puppet.Interpreter.Type
import Foreign.Ruby

instance FromRuby ResolvedValue where
    fromRuby v = do
        json <- fromRuby v
        case json of
            Just h -> return (fromJSON h)
            Nothing -> return Nothing

instance ToRuby ResolvedValue where
    toRuby = toRuby . toJSON
