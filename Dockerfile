FROM haskell:8.0

WORKDIR /usr/src/monopoly

COPY monopoly.cabal Setup.hs LICENSE ./

RUN cabal update && cabal install --only-dep

COPY src src

RUN cabal configure && cabal build
