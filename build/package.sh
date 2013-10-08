#!/bin/bash
VERSION=`grep '^version:' language-puppet.cabal | awk '{print $2}'`

if [ -z "$VERSION" ]
then
    exit -1
fi

rm -rf puppetresources-$VERSION && \
mkdir puppetresources-$VERSION && \
cabal configure -p && \
cabal build && \
cp dist/build/puppetresources/puppetresources ../language-puppet/ruby/calcerb.rb puppetresources-$VERSION && \
fakeroot tar cfvz puppetresources-$VERSION.tar.gz puppetresources-$VERSION

rm -rf puppetresources-$VERSION && \
mkdir puppetresources-$VERSION && \
cabal configure -p --enable-executable-profiling && \
cabal build && \
cp dist/build/puppetresources/puppetresources ../language-puppet/ruby/calcerb.rb puppetresources-$VERSION && \
fakeroot tar cfvz puppetresources-$VERSION-prof.tar.gz puppetresources-$VERSION

rm -rf puppetresources-$VERSION
