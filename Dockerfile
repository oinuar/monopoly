FROM tehnix/ghcjs-docker:lts-9.21

ENV PATH="/root/.stack/programs/x86_64-linux/ghcjs-0.2.1.9009021_ghc-8.0.2/bin:${PATH}"

RUN npm install --unsafe-perm -g closurecompiler

WORKDIR /usr/src/monopoly

COPY monopoly.cabal Setup.hs LICENSE ./

RUN echo "compiler: ghcjs" > cabal.config &&\
    cabal update &&\
    cabal install --only-dep

COPY src src

RUN cabal configure --ghc-options=-DGHCJS_BROWSER
 
#ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=all.js.externs
