module Log.Log where

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

-- O nome do nosso arquivo de log
logFile :: FilePath
logFile = "system.log"

-- Recebe um Nível (ex: "INFO") e uma Mensagem e escreve no arquivo de log.
logMessage :: String -> String -> IO ()
logMessage nivel msg = do
  -- pega a data e hora atuais;
  timestamp <- getCurrentTime
  -- formata a data e hora para uma string legível;
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  -- monta a linha do log;
  let logLine = formattedTime ++ " [" ++ nivel ++ "] " ++ msg ++ "\n" -- '\n' para pular a linha
  -- adiciona a linha no final do arquivo de log, por isso o appendFile;
  appendFile logFile logLine
  
imprimirLog :: IO ()
imprimirLog = do
  putStrLn "\n==================="
  putStrLn "CONTEÚDO DO LOG"
  putStrLn "==================="
  conteudo <- readFile "system.log"
  putStrLn conteudo
  putStrLn "==================="
