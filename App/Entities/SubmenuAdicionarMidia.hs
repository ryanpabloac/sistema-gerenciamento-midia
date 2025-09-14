module Entities.SubmenuAdicionarMidia where

import Tipos
import Entities.Midias
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

-- função auxiliar para listar as mídias de forma legível;
listarMidias :: [Midia] -> IO ()
listarMidias [] = putStrLn "Nenhuma mídia cadastrada."
listarMidias midias = do
  putStrLn "\n--- Lista de Mídias Cadastradas ---"
  -- 'mapM_' aplica uma ação IO (print) a cada item de uma lista;
  mapM_ print midias
  putStrLn "------------------------------------"

-- A função que imprime o menu (a sua função original)
midiaMenu :: IO ()
midiaMenu = do
  putStrLn "\n====================="
  putStrLn "Gerenciamento de Itens"
  putStrLn "====================="
  putStrLn "1 - Adicionar novo item"
  putStrLn "2 - Remover item"
  putStrLn "3 - Listar itens cadastrados"
  putStrLn "4 - Voltar ao menu principal"
  putStr "Escolha uma opção: "

-- A função de loop principal para este submenu
-- Recebe o estado atual ([Midia]) e retorna o novo estado (IO [Midia])
loopSubmenuMidias :: [Midia] -> IO [Midia]
loopSubmenuMidias listaDeMidias = do
  midiaMenu
  opcao <- getLine

  case opcao of
    "1" -> do
      novaMidia <- cadastrarMidia listaDeMidias
      let novaLista = addItem novaMidia listaDeMidias
      putStrLn "Item adicionado com sucesso!"
      loopSubmenuMidias novaLista

    "2" -> do
      -- função 'removerItem_IO' já cuida de tudo e retorna a nova lista;
      novaLista <- removerItem_IO listaDeMidias
      loopSubmenuMidias novaLista

    "3" -> do
      listarMidias listaDeMidias
      loopSubmenuMidias listaDeMidias

    "4" -> do
      putStrLn "Voltando ao menu principal..."
      return listaDeMidias

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      loopSubmenuMidias listaDeMidias
