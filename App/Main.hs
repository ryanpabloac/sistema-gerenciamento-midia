module Main where

-- Importa os tipos de dados
import Tipos
import Entities.Usuarios
import Entities.Midias
import Log.Log
import System.IO
import Servicos.Arquivos
import Entities.SubmenuAdicionarMidia

mainLoop :: [Midia] -> [Usuario] -> IO ()
mainLoop midias usuarios = do
  putStrLn "\n--- MENU PRINCIPAL ---"
  putStrLn "1 - Gerenciar Mídias"
  putStrLn "2 - Gerenciar Usuários"
  putStrLn "3 - Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine

  case opcao of
    "1" -> do
      -- Chama o loop do submenu e passa a lista de mídias atual
      novasMidias <- loopSubmenuMidias midias
      mainLoop novasMidias usuarios
    "2" -> do
      putStrLn "Submenu de usuários ainda não implementado."
      mainLoop midias usuarios
    "3" -> putStrLn "Saindo..."
    _   -> do
      putStrLn "Opção inválida."
      mainLoop midias usuarios

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  
  listaDeMidias <- carregarMidias "Arquivos/Midia.csv"
  listaDeUsuarios <- carregarUsuarios "Arquivos/Users.csv"
  
  mainLoop listaDeMidias listaDeUsuarios

{-
main :: IO ()
main = do
  putStrLn "--- INICIANDO TESTE DE CARREGAMENTO DE ARQUIVOS CSV ---"

  -- Define os caminhos corretos para os arquivos CSV
  let caminhoMidias = "Arquivos/Midia.csv"
  let caminhoUsuarios = "Arquivos/Users.csv"

  -- Chama a função para carregar as mídias
  listaDeMidias <- carregarMidias caminhoMidias
  
  -- Imprime os resultados de Mídias
  putStrLn "\n--- Mídias Carregadas ---"
  if null listaDeMidias
    then putStrLn "Nenhuma mídia foi carregada."
    else mapM_ print listaDeMidias -- 'mapM_ print' imprime cada item da lista em uma nova linha

  -- Chama a função para carregar os usuários
  listaDeUsuarios <- carregarUsuarios caminhoUsuarios
  
  -- Imprime os resultados de Usuários
  putStrLn "\n--- Usuários Carregados ---"
  if null listaDeUsuarios
    then putStrLn "Nenhum usuário foi carregado."
    else mapM_ print listaDeUsuarios

  putStrLn "\n--- TESTE CONCLUÍDO ---"
  
-}
