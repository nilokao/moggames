FROM haskell:latest

WORKDIR /app

COPY mogoso.cabal .
COPY hie.yaml .

RUN cabal update

COPY . .

RUN cabal build

CMD ["cabal", "run", "mogoso"]
