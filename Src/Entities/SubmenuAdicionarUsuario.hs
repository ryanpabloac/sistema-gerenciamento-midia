module Entities.SubmenuAdicionarUsuario where
import Tipos
import Entities.Usuarios

-- mesmas funçoes de midia;

listarUsuarios :: [Usuario] -> IO ()
listarUsuarios [] = putStrLn "Nenhum usuário cadastrado."
listarUsuarios usuarios = do
  putStrLn "\n--- Lista de Usuários Cadastrados ---"
  mapM_ print usuarios
  putStrLn "---------------------------------------"

adicionarUsuarioMenu :: IO ()
adicionarUsuarioMenu = do
  putStrLn "\n======================="
  putStrLn "Gerenciamento de Usuários"
  putStrLn "======================="
  putStrLn "1 - Adicionar novo usuário"
  putStrLn "2 - Remover usuário"
  putStrLn "3 - Listar usuários cadastrados"
  putStrLn "4 - Voltar ao menu principal"
  putStr "Escolha uma opção: "

loopSubmenuUsuarios :: [Usuario] -> IO [Usuario]
loopSubmenuUsuarios listaDeUsuarios = do
  adicionarUsuarioMenu
  opcao <- getLine

  case opcao of
    "1" -> do
      novoUsuario <- cadastrarUsuario listaDeUsuarios
      let novaLista = addUsuario novoUsuario listaDeUsuarios
      putStrLn "Usuário adicionado com sucesso!"
      loopSubmenuUsuarios novaLista

    "2" -> do
      novaLista <- removerUsuario_IO listaDeUsuarios
      loopSubmenuUsuarios novaLista

    "3" -> do
      listarUsuarios listaDeUsuarios
      loopSubmenuUsuarios listaDeUsuarios

    "4" -> do
      putStrLn "Voltando ao menu principal..."
      return listaDeUsuarios

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      loopSubmenuUsuarios listaDeUsuarios
