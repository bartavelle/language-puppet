language-puppet
===============

A library to work with Puppet manifests, test them and eventually replace everything ruby.

Basic usage
===========

    puppetresources -p /where/your/puppet/files/are -o node.name.com

Easy build instructions
=======================

It's as simple as :

    cd language-puppet
    cabal update
    cabal sandbox init
    cabal install -j -p
    
There are also [binary packages](http://lpuppet.banquise.net/download/) available.

