module Services.Movimentacoes where

import Data.List (find)
import Data.Char (toLower)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.List (findIndex)
import Entities.Tipos
import Log.Log
import Entities.Historico (registrarOperacao)

realizarEmprestimo :: [Usuario] -> [Midia] -> [Emprestimo] -> [ListaEspera] -> Matricula -> Codigo -> IO (Maybe Emprestimo, [ListaEspera])
realizarEmprestimo usuarios midias emprestimosAtivos fila matStr codigo = do
  logMessage "INFO" "Iniciando processo de empréstimo."

  let maybeUsuario = find (\u -> matricula u == matStr) usuarios
  let maybeMidia = find (\m -> cod m == codigo) midias

  case (maybeUsuario, maybeMidia) of
    (Just usuario, Just midia) -> do
      logMessage "INFO" ("Usuário e mídia encontrados. Verificando disponibilidade da mídia: " ++ titulo midia)

      let midiaJaEmprestada = any (\e -> cod (emprestimoMidia e) == codigo) emprestimosAtivos

      if midiaJaEmprestada
      then do
           putStrLn "Mídia já emprestada. Deseja entrar na lista de espera? (S/N)"
           resposta <- getLine
           if map toLower resposta == "s"
           then do
                let novaFila = adicionarNaEspera usuario midia fila
                putStrLn "Você foi adicionado à lista de espera."
                return (Nothing, novaFila)
           else return (Nothing, fila)
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
          return (Just novoEmprestimo, fila)

    (Nothing, _) -> do
      logMessage "WARN" ("Tentativa de empréstimo com matrícula inexistente: " ++ matStr)
      putStrLn "ERRO: Usuário não encontrado."
      return (Nothing,fila)

    (_, Nothing) -> do
      logMessage "WARN" ("Tentativa de empréstimo com código de mídia inexistente: " ++ show codigo)
      putStrLn "ERRO: Mídia não encontrada."
      return (Nothing, fila)


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
      

adicionarNaEspera :: Usuario -> Midia -> [ListaEspera] -> [ListaEspera]
adicionarNaEspera usuario midia fila =
  case find (\f -> cod (itemEmEspera f) == cod midia) fila of
    Just filaExistente ->
      map (\f -> if cod (itemEmEspera f) == cod midia
                 then f { usuariosNaFila = usuariosNaFila f ++ [usuario] }
                 else f) fila
    Nothing ->
      ListaEspera midia [usuario] : fila

removerDaEspera :: Midia -> [ListaEspera] -> (Maybe Usuario, [ListaEspera])
removerDaEspera midia filas =
  case find (\f -> cod (itemEmEspera f) == cod midia) filas of
    Just fila ->
      case usuariosNaFila fila of
        []     -> (Nothing, filas)
        (x:xs) ->
          let novasFilas = map (\f -> if cod (itemEmEspera f) == cod midia
                                      then f { usuariosNaFila = xs }
                                      else f) filas
          in (Just x, novasFilas)
    Nothing -> (Nothing, filas)

consultarEspera :: Midia -> [ListaEspera] -> [Usuario]
consultarEspera midia filas =
  case find (\f -> cod (itemEmEspera f) == cod midia) filas of
    Just fila -> usuariosNaFila fila
    Nothing   -> []

proximoDaEspera :: Midia -> [ListaEspera] -> Maybe Usuario
proximoDaEspera midia filas =
  case consultarEspera midia filas of
    []    -> Nothing
    (x:_) -> Just x
