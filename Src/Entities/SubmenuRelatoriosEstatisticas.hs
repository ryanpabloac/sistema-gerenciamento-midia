module Entities.SubmenuRelatoriosEstatisticas where

import Entities.Tipos
import Entities.Emprestimos
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))


-- NOTE: Necessidade de passar a lista de emprestimos feita na aba de movimentação;
relatorio :: [Emprestimo] -> [ListaEspera] -> IO ()
relatorio listaDeEmprestimos listaDeEspera = do
  putStrLn "\n======================="
  putStrLn "Relatórios e Estatísticas"
  putStrLn "======================="
  putStrLn "1 - Empréstimos ativos (por categoria)"
  putStrLn "2 - Usuários mais ativos"
  putStrLn "3 - Itens mais emprestados"
  putStrLn "4 - Frequência de empréstimos por período"
  putStrLn "5 - Itens com lista de espera"
  putStrLn "6 - Relatório de operações (por usuário / tipo de item)"
  putStrLn "7 - Voltar ao menu principal"
  putStr "Escolha uma opção: "
  
  opcao <- getLine
  loopSubmenuRelatorio listaDeEmprestimos listaDeEspera opcao

loopSubmenuRelatorio :: [Emprestimo] -> [ListaEspera] -> String -> IO ()
loopSubmenuRelatorio listaDeEmprestimos listaDeEspera opcao = do
  case opcao of
    "1" -> do
      emprestimosAtivos listaDeEmprestimos
      relatorio listaDeEmprestimos listaDeEspera
    "2" -> do
      usuariosMaisAtivos listaDeEmprestimos
      relatorio listaDeEmprestimos listaDeEspera
    "3" -> do
      itensMaisEmprestados listaDeEmprestimos
      relatorio listaDeEmprestimos listaDeEspera
    "4" -> do
      frequenciaEmprestimos listaDeEmprestimos
      relatorio listaDeEmprestimos listaDeEspera
    "5" -> do
      itensComListaEspera listaDeEspera
      relatorio listaDeEmprestimos listaDeEspera
    "6" -> do
      relatorioOperacoes listaDeEmprestimos
      relatorio listaDeEmprestimos listaDeEspera
    "7" -> do
      putStrLn "Voltando ao menu principal."
     _ -> do
      putStrLn "Opção inválida. Tente novamente."
      relatorio listaDeEmprestimos listaDeEspera
