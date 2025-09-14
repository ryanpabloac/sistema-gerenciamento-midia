module Entities.SubmenuEditar where
import Tipos
import Entities.Midias
import Entities.Usuarios

-- função que imprime o menu
edicaoMenu :: IO ()
edicaoMenu = do
  putStrLn "\n====================="
  putStrLn "Edição de Dados"
  putStrLn "====================="
  putStrLn "1 - Editar item de mídia"
  putStrLn "2 - Editar usuário"
  putStrLn "3 - Voltar ao menu principal"
  putStr "Escolha uma opção: "

-- recebe e retorna um tupla de listas: ([Midia], [Usuario])
loopSubmenuEditar :: [Midia] -> [Usuario] -> IO ([Midia], [Usuario])
loopSubmenuEditar listaDeMidias listaDeUsuarios = do
  edicaoMenu
  opcao <- getLine

  case opcao of
    "1" -> do
      -- chama a função de edição de mídias, que retorna a lista de mídias atualizada;
      novasMidias <- editarMidia_IO listaDeMidias
      -- continua o loop, passando a nova lista de mídias e a antiga de usuários;
      loopSubmenuEditar novasMidias listaDeUsuarios

    "2" -> do
      -- mesma logica do de midias; 
      novosUsuarios <- editarUsuario_IO listaDeUsuarios
      loopSubmenuEditar listaDeMidias novosUsuarios

    "3" -> do
      putStrLn "Voltando ao menu principal..."
      -- sai do loop e retorna a tupla com o estado atual das duas listas;
      return (listaDeMidias, listaDeUsuarios)

    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      loopSubmenuEditar listaDeMidias listaDeUsuarios
