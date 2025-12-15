{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static

import Control.Monad.IO.Class (liftIO)
import Data.IORef

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, encode, decode)

import System.Directory
  ( doesFileExist
  , createDirectoryIfMissing
  )

import qualified Data.ByteString.Lazy as B

--------------------------------------------------
-- estado do usuário
--------------------------------------------------

data EstadoUsuario = EstadoUsuario
  { tentativas :: [String]
  , resultados :: [[Int]]
  , gameStatus :: String
  } deriving (Show, Generic)

instance ToJSON EstadoUsuario
instance FromJSON EstadoUsuario

--------------------------------------------------
-- arquivos
--------------------------------------------------

dataDir :: FilePath
dataDir = "data"

userFile :: String -> FilePath
userFile uid = dataDir ++ "/" ++ uid ++ ".json"

--------------------------------------------------
-- palavras
--------------------------------------------------

myWords :: IO [TL.Text]
myWords = fmap TL.lines (TLIO.readFile "palavras.txt")

wordOfTheDay :: [TL.Text] -> IO TL.Text
wordOfTheDay ws = do
  today <- utctDay <$> getCurrentTime
  let (y,m,d) = toGregorian today
      idx = (fromIntegral y + fromIntegral m + fromIntegral d) `mod` length ws
  return (ws !! idx)

--------------------------------------------------
-- lógica do jogo
--------------------------------------------------

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne c (x:xs)
  | c == x    = xs
  | otherwise = x : removeOne c xs

testWord :: String -> String -> [Int]
testWord tentativa word =
  let greens   = [ if t == p then 2 else -1 | (t,p) <- zip tentativa word ]
      restWord = [ p | (g,p) <- zip greens word, g /= 2 ]
  in mark greens tentativa restWord

mark :: [Int] -> String -> String -> [Int]
mark [] [] _ = []
mark (g:gs) (t:ts) rest
  | g == 2        = 2 : mark gs ts rest
  | t `elem` rest = 1 : mark gs ts (removeOne t rest)
  | otherwise     = 0 : mark gs ts rest

--------------------------------------------------
-- estado persistente
--------------------------------------------------

loadUser :: String -> IO EstadoUsuario
loadUser uid = do
  createDirectoryIfMissing True dataDir
  let path = userFile uid
  exists <- doesFileExist path
  if exists
    then do
      bs <- B.readFile path
      case decode bs of
        Just estado -> return estado
        Nothing     -> return $ EstadoUsuario [] [] "jogando"
    else return $ EstadoUsuario [] [] "jogando"

saveUser :: String -> EstadoUsuario -> IO ()
saveUser uid estado =
  B.writeFile (userFile uid) (encode estado)

--------------------------------------------------
-- main
--------------------------------------------------

main :: IO ()
main = do
  ws <- myWords
  palavraDoDia <- wordOfTheDay ws
  ref <- newIORef palavraDoDia

  scotty 3001 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/play" $
      file "static/mogoso.html"

    options "/mogoso" $ do
      setHeader "Access-Control-Allow-Origin" "*"
      setHeader "Access-Control-Allow-Methods" "GET,POST,OPTIONS"
      setHeader "Access-Control-Allow-Headers" "Content-Type"
      text ""

    -- GET: estado do usuário
    get "/mogoso" $ do
      uidTxt <- param "userId" :: ActionM TS.Text
      let uid = TS.unpack uidTxt
      estado <- liftIO $ loadUser uid
      json estado

    -- POST: tentativa
    post "/mogoso" $ do
      uidTxt    <- formParam "userId"    :: ActionM TS.Text
      tentativa <- formParam "tentativa" :: ActionM TL.Text

      palavra <- liftIO $ readIORef ref
      ws2     <- liftIO myWords

      let uid          = TS.unpack uidTxt
          tentativaStr = map toLower . TL.unpack . TL.strip $ tentativa
          wordStr      = map toLower (TL.unpack palavra)
          wsLower      = map (map toLower . TL.unpack) ws2

      if tentativaStr `notElem` wsLower
        then json ("Palavra inválida!" :: String)
        else do
          estado <- liftIO $ loadUser uid

          let resultado = testWord tentativaStr wordStr
              novasTent = tentativas estado ++ [tentativaStr]
              novosRes  = resultados estado ++ [resultado]
              status
                | tentativaStr == wordStr = "venceu"
                | length novasTent >= 7   = "perdeu"
                | otherwise               = "jogando"

              novoEstado = EstadoUsuario novasTent novosRes status

          liftIO $ saveUser uid novoEstado
          json novoEstado
