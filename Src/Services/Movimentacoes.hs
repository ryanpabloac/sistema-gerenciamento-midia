module Services.Movimentacoes where

import Data.List (find)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Entities.Midias
import Entities.Usuarios
import Entities.Tipos
import Log.Log

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
