FROM haskell:9.6-slim

WORKDIR /app

COPY . .

RUN cabal update
RUN cabal build

CMD ["cabal", "run", "mogoso"]
