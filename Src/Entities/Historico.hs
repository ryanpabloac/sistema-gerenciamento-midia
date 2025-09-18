module Entities.Historico where

import Entities.Tipos
import Data.Time
import System.Directory (doesFileExist)

-- | Registra uma operação no histórico (Ex: "EMPRÉSTIMO" ou "DEVOLUÇÃO").
registrarOperacao :: String -> Usuario -> Midia -> IO ()
registrarOperacao tipo usuario midia = do
  horaAtual <- getCurrentTime
  appendFile "Src/Services/Arquivos/historico.txt" $
    formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" horaAtual ++
    " - [" ++ tipo ++ "] Usuario: " ++ nome usuario ++
    " (Matrícula: " ++ matricula usuario ++ ")" ++
    " | Midia: \"" ++ titulo midia ++ "\" (Código: " ++ show (cod midia) ++ ")\n"

-- | Mostra todo o histórico gravado em arquivo.
mostrarHistorico :: IO ()
mostrarHistorico = do
  putStrLn "\n===== Histórico de Operações ====="
  existe <- doesFileExist "Src/Services/Arquivos/historico.txt"
  if not existe
    then putStrLn "Nenhum histórico registrado ainda."
    else do
      conteudo <- readFile "Src/Services/Arquivos/historico.txt"
      if null conteudo
        then putStrLn "Nenhuma operação registrada até o momento."
        else putStrLn conteudo
