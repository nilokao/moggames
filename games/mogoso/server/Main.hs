{-# LANGUAGE OverloadedStrings #-}

module Main (main, testWord, mark, removeOne, myWords, getRandomWord) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static 
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

-- sorteia palavra
getRandomWord :: IO TL.Text
getRandomWord = do
    ws <- myWords
    index <- randomRIO (0, length ws - 1)
    return (ws !! index)

-- remove uma letra (pra repetição)
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne c (x:xs)
    | c == x    = xs
    | otherwise = x : removeOne c xs

-- marca verdes
testWord :: String -> String -> [Int]
testWord tentativa word =
    let greens   = [ if t == p then 2 else -1 | (t,p) <- zip tentativa word ]
        restWord = [ p | (g,p) <- zip greens word, g /= 2 ]
    in mark greens tentativa restWord

-- marca 0/1/2
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

    scotty 3001 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (addBase "static")


        -- rota do frontend
        get "/play" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            file "static/mogoso.html"

        -- CORS básico
        options "/mogoso" $ do
            setHeader "Access-Control-Allow-Origin" "*"
            setHeader "Access-Control-Allow-Methods" "GET,POST,OPTIONS"
            setHeader "Access-Control-Allow-Headers" "Content-Type"
            text ""

        -- GET: retorna palavra sorteada
        get "/mogoso" $ do
            w <- liftIO (readIORef ref)
            setHeader "Access-Control-Allow-Origin" "*"
            text w

        -- POST: tenta palavra
        post "/mogoso" $ do
            tentativa <- formParam "tentativa"
            w <- liftIO (readIORef ref)
            ws <- liftIO myWords
            setHeader "Access-Control-Allow-Origin" "*"

            let tentativaStr   = TL.unpack (TL.strip tentativa)
                wordStr        = TL.unpack w
                tentativaLower = map toLower tentativaStr
                wsLower        = map (map toLower . TL.unpack) ws

            if tentativaLower `notElem` wsLower
            then json ("Palavra inválida!" :: String)
            else json (testWord tentativaStr wordStr)
