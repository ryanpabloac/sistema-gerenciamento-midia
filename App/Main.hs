module Main where

-- Importa os tipos de dados
import Tipos
import Entities.Usuarios
import Entities.Midias
import Log.Log
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
-- Importa as funções de carregamento pelo nome completo do módulo
import Servicos.Arquivos

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
