FROM haskell:9.6

WORKDIR /app

COPY games/mogoso/cabal.project* ./
COPY games/mogoso/mogoso.cabal ./
COPY games/mogoso/server ./server
COPY games/mogoso/palavras.txt ./palavras.txt

RUN cabal update
RUN cabal build

CMD ["cabal", "run", "mogoso"]
