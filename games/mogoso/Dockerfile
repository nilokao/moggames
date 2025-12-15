FROM haskell:9.6.7 AS builder

WORKDIR /app

COPY mogoso.cabal .
COPY hie.yaml .
COPY palavras.txt .
COPY static ./static

RUN cabal update
COPY . .

RUN cabal build exe:mogoso -v0

FROM debian:bullseye-slim

WORKDIR /app

COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-9.6.7/mogoso-0.1.0.0/x/mogoso/build/mogoso/mogoso .
COPY --from=builder /app/palavras.txt .

EXPOSE 3001

CMD ["./mogoso"]