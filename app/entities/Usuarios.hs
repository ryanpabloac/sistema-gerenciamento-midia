module Usuarios where
import Tipos

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
  -- Note que não precisamos de 'read', pois todos os campos são Strings.
  return Usuario
    { nome = nomeUsuario
    , matricula = matriculaUsuario
    , email = emailUsuario
    }
