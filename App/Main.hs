module Main where

import Entities.Tipos
import System.IO
import Services.Arquivos
import Entities.SubmenuAdicionarMidia
import Entities.SubmenuAdicionarUsuario
import Entities.SubmenuEditar

-- | Função principal do programa.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n=== Bem-vindo ao Sistema de Mídias ==="
  putStrLn "Carregando dados..."

  -- Carrega a lista de midias e usuarios dos arquivos CSV.
  listaDeMidias <- carregarMidias "Src/Services/Arquivos/Midia.csv"
  listaDeUsuarios <- carregarUsuarios "Src/Services/Arquivos/Users.csv"

  -- Inicia o loop principal do sistema.
  mainLoop listaDeMidias listaDeUsuarios

-- | Loop principal do sistema: exibe o menu e lida com as opções do usuário.
mainLoop :: [Midia] -> [Usuario] -> IO ()
mainLoop midias usuarios = do
  putStrLn "\n========================================"
  putStrLn "  Sistema de Mídias - Menu Principal    "
  putStrLn "========================================"
  putStrLn "1 - Cadastro de Itens"
  putStrLn "2 - Cadastro de Usuários"
  putStrLn "3 - Empréstimos e Devoluções"
  putStrLn "4 - Busca e Listagem Avançada"
  putStrLn "5 - Relatórios e Estatísticas"
  putStrLn "6 - Edição de Dados"
  putStrLn "7 - Exportação/Importação de Dados"
  putStrLn "8 - Auditoria e Histórico"
  putStrLn "0 - Salvar e Sair"
  putStrLn "----------------------------------------"
  putStr "Digite uma opção: "
  
  opcao <- getLine

  case opcao of
    "1" -> do
      novasMidias <- loopSubmenuMidias midias
      mainLoop novasMidias usuarios
    
    "2" -> do
      novosUsuarios <- loopSubmenuUsuarios usuarios
      mainLoop midias novosUsuarios

    "3" -> do
      putStrLn "\n[DEBUG] Acesso a Empréstimos e Devoluções..."
      -- Aqui você chamaria a função para o submenu de Empréstimos.
      mainLoop midias usuarios

    "4" -> do
      putStrLn "\n[DEBUG] Acesso a Busca e Listagem Avançada..."
      -- Aqui você chamaria a função para o submenu de Busca.
      mainLoop midias usuarios
      
    "5" -> do
      putStrLn "\n[DEBUG] Acesso a Relatórios e Estatísticas..."
      -- Aqui você chamaria a função para o submenu de Relatórios.
      mainLoop midias usuarios

    "6" -> do
      (novasMidias, novosUsuarios) <- loopSubmenuEditar midias usuarios
      mainLoop novasMidias novosUsuarios

    "7" -> do
      putStrLn "\n[DEBUG] Acesso a Exportação/Importação..."
      -- Aqui você chamaria a função para o submenu de Exportação.
      mainLoop midias usuarios

    "8" -> do
      putStrLn "\n[DEBUG] Acesso a Auditoria e Histórico..."
      -- Aqui você chamaria a função para o submenu de Auditoria.
      mainLoop midias usuarios

    "0" -> do
      putStrLn "\nSalvando alterações..."
      salvarMidias "Src/Services/Arquivos/midias.csv" midias
      salvarUsuarios "Src/Services/Arquivos/usuarios.csv" usuarios
      putStrLn "Dados salvos com sucesso. Até a próxima!"
      return ()

    _ -> do
      putStrLn "\nOpção inválida! Por favor, digite um número válido."
      mainLoop midias usuarios
