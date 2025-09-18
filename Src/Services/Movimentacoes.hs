module Services.Movimentacoes where

import Data.List (find)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.List (findIndex)
import Entities.Tipos
import Log.Log
import Entities.Historico (registrarOperacao)

realizarEmprestimo :: [Usuario] -> [Midia] -> [Emprestimo] -> Matricula -> Codigo -> IO (Maybe Emprestimo)
realizarEmprestimo usuarios midias emprestimosAtivos matStr codigo = do
  logMessage "INFO" "Iniciando processo de empréstimo."

  let maybeUsuario = find (\u -> matricula u == matStr) usuarios
  let maybeMidia = find (\m -> cod m == codigo) midias

  case (maybeUsuario, maybeMidia) of
    (Just usuario, Just midia) -> do
      logMessage "INFO" ("Usuário e mídia encontrados. Verificando disponibilidade da mídia: " ++ titulo midia)

      let midiaJaEmprestada = any (\e -> cod (emprestimoMidia e) == codigo) emprestimosAtivos

      if midiaJaEmprestada
        then do
          logMessage "WARN" "Tentativa de emprestar uma mídia já emprestada."
          putStrLn "ERRO: Esta mídia já está emprestada."
          return Nothing
        else do
          currentTime <- getCurrentTime
          let dataAtual = utctDay currentTime

          let novoEmprestimo = Emprestimo
                { emprestimoUsuario = usuario
                , emprestimoMidia = midia
                , dataEmprestimo = dataAtual
                }
          registrarOperacao "EMPRÉSTIMO" usuario midia
          logMessage "INFO" ("Novo empréstimo criado: " ++ show novoEmprestimo)
          putStrLn "Empréstimo realizado com sucesso!"
          putStrLn $ "Mídia: " ++ titulo midia ++ " emprestada para " ++ nome usuario ++ "."
          return (Just novoEmprestimo)

    (Nothing, _) -> do
      logMessage "WARN" ("Tentativa de empréstimo com matrícula inexistente: " ++ matStr)
      putStrLn "ERRO: Usuário não encontrado."
      return Nothing

    (_, Nothing) -> do
      logMessage "WARN" ("Tentativa de empréstimo com código de mídia inexistente: " ++ show codigo)
      putStrLn "ERRO: Mídia não encontrada."
      return Nothing


devolverMidia :: [Emprestimo] -> Matricula -> Codigo -> IO (Maybe [Emprestimo])
devolverMidia emprestimos matStr codigo = do
  logMessage "INFO" "Iniciando processo de devolução."
  
  -- Encontra o empréstimo a ser removido com base na matrícula do usuário e no código da mídia
  let maybeEmprestimoIndex = findIndex (\e -> matricula (emprestimoUsuario e) == matStr && cod (emprestimoMidia e) == codigo) emprestimos
  
  case maybeEmprestimoIndex of
    Just index -> do
      let emprestimoRemovido = emprestimos !! index
      logMessage "INFO" ("Devolução encontrada. Removendo o empréstimo: " ++ show emprestimoRemovido)
      
      -- Remove o empréstimo da lista usando a função de filtro
      let novaListaEmprestimos = filter (\e -> e /= emprestimoRemovido) emprestimos
      registrarOperacao "DEVOLUÇÃO" (emprestimoUsuario emprestimoRemovido) (emprestimoMidia emprestimoRemovido)      
      putStrLn "Devolução realizada com sucesso!"
      putStrLn $ "Mídia: " ++ titulo (emprestimoMidia emprestimoRemovido) ++ " devolvida por " ++ nome (emprestimoUsuario emprestimoRemovido) ++ "."
      return (Just novaListaEmprestimos)
    
    Nothing -> do
      logMessage "WARN" ("Tentativa de devolução de uma mídia não emprestada: " ++ matStr ++ ", " ++ show codigo)
      putStrLn "ERRO: Empréstimo não encontrado para este usuário e mídia."
      return Nothing
