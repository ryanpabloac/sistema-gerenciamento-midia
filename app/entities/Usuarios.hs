module Entities.Usuarios where
import Tipos
import Data.List
import Data.Char

addUsuario :: Usuario -> [Usuario] -> [Usuario]
addUsuario novoUsuario lista = novoUsuario : lista

buscarUsuarioPorMatricula :: Matricula -> [Usuario] -> Maybe Usuario
buscarUsuarioPorMatricula mat_busca lista =
  -- A lógica é a mesma do buscaItem
  find (\user -> matricula user == mat_busca) lista

removerUsuarioPorMatricula :: Matricula -> [Usuario] -> [Usuario]
removerUsuarioPorMatricula mat_remover lista =
  -- A lógica é a mesma do buscaItem
  filter (\user -> matricula user /= mat_remover) lista

cadastrarUsuario :: IO Usuario
cadastrarUsuario = do
  putStrLn "--- Cadastro de Novo Usuário ---"
  
  putStr "Digite o nome: "
  nomeUsuario <- getLine

  putStr "Digite a matrícula: "
  matriculaUsuario <- getLine

  putStr "Digite o email: "
  emailUsuario <- getLine

  -- 'return' embrulha o 'Usuario' criado em uma ação IO.
  -- OBS: nao tem read pq tudo é string;
  return Usuario
    { nome = nomeUsuario
    , matricula = matriculaUsuario
    , email = emailUsuario
    }
    
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
      putStr "Informe novo nome: "
      novoNome <- getLine
      -- Retorna uma cópia do usuário com o campo 'nome' alterado;
      return usuarioOriginal { nome = novoNome }
    "2" -> do
      putStr "Informe novo e-mail: "
      novoEmail <- getLine
      -- mesma coisa do nome;
      return usuarioOriginal { email = novoEmail }
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      edicaoUsuario usuarioOriginal

-- Função principal que orquestra todo o processo de edição
editarUsuario_IO :: [Usuario] -> IO [Usuario]
editarUsuario_IO listaDeUsuarios = do
  putStrLn "-----------"
  putStrLn "Editar usuário"
  putStrLn "-----------"
  putStr "Informe a matrícula: "
  mat <- getLine

  case buscarUsuarioPorMatricula mat listaDeUsuarios of -- case chama a função que busca usuario;
    -- nothing e just pq a função de busca é do tipo maybe
    Nothing -> do
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
          putStrLn "Sucesso! Usuário atualizado."
          -- AQUI SERIA O LOCAL DE REGISTRAR UM LOG --
          return novaLista
        _ -> do
          putStrLn "Edição cancelada."
          return listaDeUsuarios
