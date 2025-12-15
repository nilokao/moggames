FROM haskell:9.2.7

WORKDIR /app

COPY mogoso.cabal .
COPY hie.yaml .
COPY . .

RUN cabal update
RUN cabal build

CMD ["cabal", "run", "mogoso"]
