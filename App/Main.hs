module Main where

-- Importa os tipos de dados
import Tipos
import Entities.Usuarios
import Entities.Midias
import Log.Log
import System.IO
import Servicos.Arquivos
import Entities.SubmenuAdicionarMidia
import Entities.SubmenuAdicionarUsuario
import Entities.SubmenuEditar

mainLoop :: [Midia] -> [Usuario] -> IO ()
mainLoop midias usuarios = do
  putStrLn "\n--- MENU PRINCIPAL ---"
  putStrLn "1 - Gerenciar Mídias (Adicionar/Remover/Listar)"
  putStrLn "2 - Gerenciar Usuários (Adicionar/Remover/Listar)"
  putStrLn "3 - Editar Dados (Mídias ou Usuários)"
  putStrLn "4 - Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine

  case opcao of
    "1" -> do
      novasMidias <- loopSubmenuMidias midias
      mainLoop novasMidias usuarios
      
    "2" -> do
      novosUsuarios <- loopSubmenuUsuarios usuarios
      mainLoop midias novosUsuarios

    "3" -> do
      (novasMidias, novosUsuarios) <- loopSubmenuEditar midias usuarios
      mainLoop novasMidias novosUsuarios

    "4" -> do
      putStrLn "Salvando alterações nos arquivos..."
      salvarMidias "Arquivos/midias.csv" midias
      salvarUsuarios "Arquivos/usuarios.csv" usuarios
      putStrLn "Dados salvos com sucesso. Saindo..."
      putStrLn "Saindo..."
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
