module Entities.Usuarios where
import Tipos
import Log.Log
import Data.List
import Data.Char

addUsuario :: Usuario -> [Usuario] -> [Usuario]
addUsuario novoUsuario lista = novoUsuario : lista

-- verifica se a matrícula já existe;
validaMatriculaUnica :: Matricula -> [Usuario] -> Either String ()
validaMatriculaUnica mat lista =
  if any (\u -> matricula u == mat) lista
  then Left $ "Erro: matrícula \"" ++ mat ++ "\" já cadastrada."
  else Right ()

-- verifica se o e-mail tem um formato correto ('@' e '.' depois do '@').
validaEmail :: Email -> Either String ()
validaEmail email =
  case elemIndex '@' email of
    Nothing -> Left $ "Erro: e-mail \"" ++ email ++ "\" está mal formatado (falta @)."
    Just indexArroba ->
      if isInfixOf "." (drop (indexArroba + 1) email)
      then Right ()
      else Left $ "Erro: e-mail \"" ++ email ++ "\" está mal formatado (falta '.' após o domínio)."

-- Função mestre que combina todas as validações de usuário.
-- O '>>' encadeia as validações, parando na primeira que der erro.
validaNovoUsuario :: Matricula -> Email -> [Usuario] -> Either String ()
validaNovoUsuario mat email lista =
  validaMatriculaUnica mat lista >>
  validaEmail email

buscarUsuarioPorMatricula :: Matricula -> [Usuario] -> Maybe Usuario
buscarUsuarioPorMatricula mat_busca lista =
  -- A lógica é a mesma do buscaItem
  find (\user -> matricula user == mat_busca) lista

removerUsuarioPorMatricula :: Matricula -> [Usuario] -> [Usuario]
removerUsuarioPorMatricula mat_remover lista =
  -- A lógica é a mesma do buscaItem
  filter (\user -> matricula user /= mat_remover) lista
  
removerUsuario_IO :: [Usuario] -> IO [Usuario]
removerUsuario_IO listaDeUsuarios = do
  logMessage "INFO" "Sessão de remoção de usuário iniciada." -- << LOG
  putStrLn "-----------"
  putStrLn "Remover Usuário"
  putStrLn "-----------"
  putStr "Informe a matrícula do usuário a ser removido: "
  mat <- getLine

  case buscarUsuarioPorMatricula mat listaDeUsuarios of
    Nothing -> do
      logMessage "WARN" ("Tentativa de remover usuário com matrícula inexistente: " ++ mat) -- log
      putStrLn "ERRO: Usuário não encontrado."
      return listaDeUsuarios
    Just usuarioParaRemover -> do
      putStrLn $ "Você está prestes a remover o usuário: \"" ++ nome usuarioParaRemover ++ "\"."
      putStr "Confirma remoção? (S/N): "
      confirmacao <- getLine
      case map toLower confirmacao of
        "s" -> do
          let novaLista = removerUsuarioPorMatricula mat listaDeUsuarios
          logMessage "INFO" ("Usuário com matrícula '" ++ mat ++ "' foi REMOVIDO.") -- log
          putStrLn "Sucesso! Usuário removido."
          return novaLista
        _ -> do
          logMessage "INFO" ("Remoção do usuário com matrícula '" ++ mat ++ "' foi cancelada.") -- log
          putStrLn "Remoção cancelada."
          return listaDeUsuarios

cadastrarUsuario :: [Usuario] -> IO Usuario
cadastrarUsuario listaDeUsuarios = do
  logMessage "INFO" "Sessão de cadastro de novo usuário iniciada."
  putStrLn "--- Cadastro de Novo Usuário ---"
  
  putStr "Digite o nome: "
  nomeUsuario <- getLine

  putStr "Digite a matrícula: "
  matriculaUsuario <- getLine

  putStr "Digite o email: "
  emailUsuario <- getLine

  --logMessage "INFO" ("Novo usuário '" ++ nomeUsuario ++ "' criado em memória, aguardando adição à lista principal.")
  
   -- <<< ETAPA DE VALIDAÇÃO >>>
  case validaNovoUsuario matriculaUsuario emailUsuario listaDeUsuarios of
    Left erro -> do
      -- se a validação falhou, mostra o erro e tenta o cadastro novamente;
      putStrLn erro
      cadastrarUsuario listaDeUsuarios
    Right () -> do
      -- se a validação passou, cria e retorna o usuário;
      logMessage "INFO" ("Novo usuário '" ++ nomeUsuario ++ "' criado com sucesso.")
      let novoUsuario = Usuario { nome = nomeUsuario, matricula = matriculaUsuario, email = emailUsuario }
      return novoUsuario
      -- 'return' embrulha o 'Usuario' criado em uma ação IO.
      -- OBS: nao tem read pq tudo é string;
    
-- função auxiliar para mostrar os dados de um usuário;
mostrarUsuario :: Usuario -> IO ()
mostrarUsuario usr = do
  putStrLn $ "Dados atuais:"
  putStrLn $ "Nome: " ++ nome usr
  putStrLn $ "Matrícula: " ++ matricula usr
  putStrLn $ "E-mail: " ++ email usr

-- função que gerencia o menu de edição e retorna o usuário modificado;
edicaoUsuario :: Usuario -> IO Usuario
edicaoUsuario usuarioOriginal = do
  putStrLn "Escolha campo para editar:"
  putStrLn "1 - Nome"
  putStrLn "2 - E-mail"
  putStr "Opção: "
  opcao <- getLine

  case opcao of
    "1" -> do
      logMessage "DEBUG" ("Usuário escolheu editar o campo 'Nome'.")
      putStr "Informe novo nome: "
      novoNome <- getLine
      -- Retorna uma cópia do usuário com o campo 'nome' alterado;
      return usuarioOriginal { nome = novoNome }
    "2" -> do
      logMessage "DEBUG" ("Usuário escolheu editar o campo 'E-mail'.")
      putStr "Informe novo e-mail: "
      novoEmail <- getLine
      -- mesma coisa do nome;
      return usuarioOriginal { email = novoEmail }
    _ -> do
      logMessage "WARN" ("Usuário inseriu uma opção de edição inválida: '" ++ opcao ++ "'.")
      putStrLn "Opção inválida. Tente novamente."
      edicaoUsuario usuarioOriginal

-- Função principal que orquestra todo o processo de edição
editarUsuario_IO :: [Usuario] -> IO [Usuario]
editarUsuario_IO listaDeUsuarios = do
  logMessage "INFO" "Sessão de edição de usuário iniciada." -- log
  putStrLn "-----------"
  putStrLn "Editar usuário"
  putStrLn "-----------"
  putStr "Informe a matrícula: "
  mat <- getLine

  case buscarUsuarioPorMatricula mat listaDeUsuarios of -- case chama a função que busca usuario;
    -- nothing e just pq a função de busca é do tipo maybe
    Nothing -> do
      logMessage "WARN" ("Tentativa de editar usuário com matrícula inexistente: " ++ mat) -- log
      putStrLn "ERRO: Usuário não encontrado."
      return listaDeUsuarios -- retorna a lista original sem alterações;
    Just usuarioEncontrado -> do
      putStrLn $ "\nUsuário encontrado: " ++ nome usuarioEncontrado
      mostrarUsuario usuarioEncontrado
      
      -- pega a versao modificada do usuario;
      usuarioModificado <- edicaoUsuario usuarioEncontrado
      
      putStr "Confirma edição? (S/N): "
      confirmacao <- getLine

      case map toLower confirmacao of
        "s" -> do
          -- Cria a nova lista substituindo o usuario antigo pelo modificado
          let novaLista = map (\u -> if matricula u == mat then usuarioModificado else u) listaDeUsuarios
          logMessage "INFO" ("Usuário de matrícula '" ++ mat ++ "' foi alterado.") -- log
          putStrLn "Sucesso! Usuário atualizado."
          -- AQUI SERIA O LOCAL DE REGISTRAR UM LOG --
          return novaLista
        _ -> do
          logMessage "INFO" ("Edição do usuário de matrícula '" ++ mat ++ "' foi cancelada pelo operador.") -- log
          putStrLn "Edição cancelada."
          return listaDeUsuarios
