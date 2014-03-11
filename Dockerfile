FROM gregweber/haskell-platform-2013.2-deb64
MAINTAINER bartavelle

RUN sudo apt-get update
RUN sudo apt-get install -yq ruby libruby ruby-dev

ENV PATH /home/haskell/.cabal/bin:$PATH

RUN cabal update
RUN cabal install -j -p language-puppet hruby

