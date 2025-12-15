FROM haskell:9.6 as builder
WORKDIR /app

COPY server/ ./server
COPY mogoso.cabal . 
COPY palavras.txt . 

WORKDIR /app
RUN cabal update
RUN cabal build

FROM debian:bullseye-slim
WORKDIR /app

COPY --from=builder /app/dist-newstyle/build/*/x86_64-linux/ghc-9.6.7/mogoso-0.1.0.0/x/mogoso/build/mogoso/mogoso ./mogoso

COPY static ./static
COPY palavras.txt ./

EXPOSE 3001

CMD ["./mogoso"]
