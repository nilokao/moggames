{-# LANGUAGE OverloadedStrings #-}

module Main (main, testWord, mark, removeOne, myWords, getRandomWord) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text.Lazy (Text)
import qualified Data.Text as TS
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Char (toLower)

-- lê lista de palavras
myWords :: IO [TL.Text]
myWords = fmap (map TL.fromStrict . TS.lines) (TIO.readFile "palavras.txt")

-- sorteia uma palavra
getRandomWord :: IO TL.Text
getRandomWord = do
    ws <- myWords
    index <- randomRIO (0, length ws - 1)
    return (ws !! index)

-- vai remover uma letra de uma string, isso vai ser usado para casos de haver uma letra repetida em uma palavra
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne c (x:xs)
    | c == x    = xs
    | otherwise = x : removeOne c xs

-- função principal de testagem de palavra, ela marca como "verdes" as letras certas e na posição certas, para que durante aquele teste elas sejam removidas
testWord :: String -> String -> [Int]
testWord tentativa word =
    let greens   = [ if t == p then 2 else -1 | (t,p) <- zip tentativa word ]
        restWord = [ p | (g,p) <- zip greens word, g /= 2 ]
    in mark greens tentativa restWord

-- função que "marca" as letras, retornando uma lista com números, 0, 1 ou 2, que representam se uma letra está errada, parcialmente correta ou correta
mark :: [Int] -> String -> String -> [Int]
mark [] [] _ = []
mark (g:gs) (t:ts) rest
    | g == 2          = 2 : mark gs ts rest
    | t `elem` rest   = 1 : mark gs ts (removeOne t rest)
    | otherwise       = 0 : mark gs ts rest

main :: IO ()
main = do
    randomWord <- getRandomWord
    ref <- newIORef randomWord

    scotty 5000 $ do
        middleware logStdoutDev

        -- Preflight CORS
        options "/mogoso" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            setHeader "Access-Control-Allow-Methods" "GET,POST,OPTIONS"
            setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
            text ""

        -- GET: retorna a palavra atual a cada abertura do servidor
        get "/mogoso" $ do
            w <- liftIO (readIORef ref)
            setHeader "Access-Control-Allow-Origin" "*"
            text w

        -- POST: recebe tentativa e compara
        post "/mogoso" $ do
            tentativa <- formParam "tentativa"
            w <- liftIO (readIORef ref)
            ws <- liftIO myWords
            setHeader "Access-Control-Allow-Origin" "*"

            -- recebe e normaliza a palavra recebida
            let tentativaStr   = TL.unpack (TL.strip tentativa)
                wordStr        = TL.unpack w
                tentativaLower = map toLower tentativaStr
                wsLower        = map (map toLower . TL.unpack) ws

            -- cria um json com o retorno do testWord e passa para o frontend, além de testar se a palavra está na base de dados
            if tentativaLower `notElem` wsLower
            then json ("Palavra inválida!" :: String)
            else do
                let result = testWord tentativaStr wordStr
                json result