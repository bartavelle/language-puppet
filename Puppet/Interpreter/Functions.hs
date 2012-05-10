module Puppet.Interpreter.Functions (fqdn_rand) where

import Data.Hash.MD5
import Data.String.Utils (join)


{-
    TODO : achieve compatibility with puppet

    fqdn_rand(1000000) -> 110616
    fqdn_rand(1000000, 'nqsdnio') -> 240966
    fqdn_rand(1000000, 'qsdqsd', 'dqsqsd') -> 731879
    fqdn_rand(1000000, 'qsdqsdqsdqsdqsd') -> 386411

Puppet::Parser::Functions::newfunction(:fqdn_rand, :type => :rvalue, :doc =>
  "Generates random numbers based on the node's fqdn. Generated random values
  will be a range from 0 up to and excluding n, where n is the first parameter.
  The second argument specifies a number to add to the seed and is optional, for example:

      $random_number = fqdn_rand(30)
      $random_number_seed = fqdn_rand(30,30)") do |args|
    require 'digest/md5'
    max = args.shift
    srand(Digest::MD5.hexdigest([lookupvar('::fqdn'),args].join(':')).hex)
    rand(max).to_s
end

-}

-- the first String must be the fqdn
fqdn_rand :: Int -> [String] -> Int
fqdn_rand n args = fromIntegral (hash `mod` (fromIntegral n))
    where
        fullstring = join ":" args
        hash = md5i $ Str fullstring

test1 = (fqdn_rand 1000000 ["glouglou.it.int",""], 110616)
test2 = (fqdn_rand 1000000 ["glouglou.it.int","nqsdnio"], 240966)
test3 = (fqdn_rand 1000000 ["glouglou.it.int","qsdqsd", "dqsqsd"], 731879)
test4 = (fqdn_rand 1000000 ["glouglou.it.int","qsdqsdqsdqsdqsd"], 386411)

main = do
    print test1
    print test2
    print test3
    print test4
