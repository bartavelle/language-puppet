module Erb.Evaluate (rubyEvaluate) where

import qualified Data.Map as Map
import Puppet.Interpreter.Types
import Erb.Ruby

rubyEvaluate :: Map.Map String GeneralValue -> String -> [RubyStatement] -> String
rubyEvaluate vars ctx = foldl (evalruby vars ctx) ""

evalruby :: Map.Map String GeneralValue -> String -> String -> RubyStatement -> String
evalruby mp ctx x st = x
