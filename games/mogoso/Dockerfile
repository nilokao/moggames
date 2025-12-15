FROM haskell:9.6

WORKDIR /app

COPY games/mogoso ./mogoso

WORKDIR /app/mogoso

RUN cabal update
RUN cabal build

CMD ["cabal", "run", "mogoso"]
