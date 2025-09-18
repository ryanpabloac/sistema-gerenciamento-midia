module Services.SubmenuMovimentacoes where

import Services.Movimentacoes
import Entities.Tipos

-- Função auxiliar para listar empréstimos ativos
listarEmprestimos :: [Emprestimo] -> IO ()
listarEmprestimos [] = putStrLn "Nenhum empréstimo ativo."
listarEmprestimos emprestimos = do
  putStrLn "\n--- Lista de Empréstimos Ativos ---"
  mapM_ print emprestimos
  putStrLn "------------------------------------"

-- A função que imprime o menu de empréstimos (a sua função original)
emprestimoMenu :: IO ()
emprestimoMenu = do
  putStrLn "\n====================="
  putStrLn "EMPRÉSTIMOS E DEVOLUÇÕES"
  putStrLn "====================="
  putStrLn "1 - Realizar Empréstimo"
  putStrLn "2 - Listar Empréstimos Ativos"
  putStrLn "3 - Realizar Devolução"
  putStrLn "4 - Voltar ao menu principal"
  putStr "Escolha uma opção: "

-- A função de loop principal para este submenu
-- Recebe o estado atual das listas e retorna a lista de empréstimos atualizada
loopEmprestimoMenu :: [Emprestimo] -> [Midia] -> [Usuario] -> IO [Emprestimo]
loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios = do
  emprestimoMenu
  opcao <- getLine

  case opcao of
    "1" -> do
      putStrLn "Digite a matrícula do usuário:"
      matStr <- getLine
      putStrLn "Digite o código da mídia:"
      codStr <- getLine
      
      let maybeCodigo = readMay codStr :: Maybe Codigo
      case maybeCodigo of
        Just codigo -> do
          -- Chama a função de empréstimo e lida com o resultado
          maybeNovoEmprestimo <- realizarEmprestimo listaDeUsuarios listaDeMidias listaDeEmprestimos matStr codigo
          case maybeNovoEmprestimo of
            Just novoEmprestimo -> do
              -- Adiciona o novo empréstimo à lista e retorna a lista atualizada
              let novaListaEmprestimos = novoEmprestimo : listaDeEmprestimos
              loopEmprestimoMenu novaListaEmprestimos listaDeMidias listaDeUsuarios
            Nothing -> do
              -- Empréstimo não foi realizado, então a lista não muda
              loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios
        Nothing -> do
          putStrLn "ERRO: Código de mídia inválido."
          loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios

    "2" -> do
      listarEmprestimos listaDeEmprestimos
      loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios
      
    "3" -> do
      putStrLn "Digite a matrícula do usuário que está devolvendo a mídia:"
      matStr <- getLine
      putStrLn "Digite o código da mídia a ser devolvida:"
      codStr <- getLine
      
      let maybeCodigo = readMay codStr :: Maybe Codigo
      case maybeCodigo of
        Just codigo -> do
          maybeNovaLista <- devolverMidia listaDeEmprestimos matStr codigo
          case maybeNovaLista of
            Just novaLista -> loopEmprestimoMenu novaLista listaDeMidias listaDeUsuarios
            Nothing -> loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios
        Nothing -> do
          putStrLn "ERRO: Código de mídia inválido."
          loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios

    "4" -> do
      putStrLn "Voltando ao menu principal..."
      return listaDeEmprestimos

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      loopEmprestimoMenu listaDeEmprestimos listaDeMidias listaDeUsuarios

-- Função auxiliar para ler um valor com segurança
readMay :: Read a => String -> Maybe a
readMay s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
