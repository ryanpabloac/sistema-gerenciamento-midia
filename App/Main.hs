module Main where

import Entities.Tipos
import System.IO
import Services.Arquivos
import Services.SubmenuMovimentacoes
import Entities.SubmenuRelatoriosEstatisticas
import Entities.SubmenuAdicionarMidia
import Entities.SubmenuAdicionarUsuario
import Entities.SubmenuEditar
import Entities.Midias
import Entities.Historico

-- | Função principal do programa.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n=== Bem-vindo ao Sistema de Mídias ==="
  putStrLn "Carregando dados..."

  -- Carrega a lista de midias e usuarios dos arquivos CSV.
  listaDeMidias <- carregarMidias "Src/Services/Arquivos/midias.csv"
  listaDeUsuarios <- carregarUsuarios "Src/Services/Arquivos/usuarios.csv"
  listaDeEmprestimos <- carregarEmprestimos "Src/Services/Arquivos/emprestimos.csv" listaDeUsuarios listaDeMidias
  filaEspera <- carregarFilas "Src/Services/Arquivos/listaEspera.csv" listaDeUsuarios listaDeMidias 

  -- Inicia o loop principal do sistema.
  mainLoop listaDeMidias listaDeUsuarios listaDeEmprestimos filaEspera

-- | Loop principal do sistema: exibe o menu e lida com as opções do usuário.
mainLoop :: [Midia] -> [Usuario] -> [Emprestimo] -> [ListaEspera] -> IO ()
mainLoop midias usuarios emprestimos fila = do
  putStrLn "\n========================================"
  putStrLn "  Sistema de Mídias - Menu Principal    "
  putStrLn "========================================"
  putStrLn "1 - Cadastro de Itens"
  putStrLn "2 - Cadastro de Usuários"
  putStrLn "3 - Empréstimos e Devoluções"
  putStrLn "4 - Busca e Listagem Avançada"
  putStrLn "5 - Relatórios e Estatísticas"
  putStrLn "6 - Edição de Dados"
  putStrLn "7 - Auditoria e Histórico"
  putStrLn "0 - Salvar e Sair"
  putStrLn "----------------------------------------"
  putStr "Digite uma opção: "

  opcao <- getLine
  
  case opcao of
    "1" -> do
      novasMidias <- loopSubmenuMidias midias
      mainLoop novasMidias usuarios emprestimos fila

    "2" -> do
      novosUsuarios <- loopSubmenuUsuarios usuarios
      mainLoop midias novosUsuarios emprestimos fila

    "3" -> do
      (novosEmprestimos, novaFila) <- loopEmprestimoMenu emprestimos midias usuarios fila
      mainLoop midias usuarios novosEmprestimos novaFila

    "4" -> do
      buscarMidias_IO midias
      mainLoop midias usuarios emprestimos fila

    "5" -> do
      relatorio emprestimos fila
      mainLoop midias usuarios emprestimos fila

    "6" -> do
      (novasMidias, novosUsuarios) <- loopSubmenuEditar midias usuarios
      mainLoop novasMidias novosUsuarios emprestimos fila

    "7" -> do
      mostrarHistorico
      mainLoop midias usuarios emprestimos fila

    "0" -> do
      putStrLn "\nSalvando alterações..."
      salvarMidias "Src/Services/Arquivos/midias.csv" midias
      salvarUsuarios "Src/Services/Arquivos/usuarios.csv" usuarios
      salvarFilas "Src/Services/Arquivos/listaEspera.csv" fila
      putStrLn "Dados salvos com sucesso. Até a próxima!"
      return ()

    _ -> do
      putStrLn "\nOpção inválida! Por favor, digite um número válido."
      mainLoop midias usuarios emprestimos fila
