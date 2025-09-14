module SubmenuAdicionarUsuario where
import Tipos

adicionarUsuarioMenu :: IO ()
adicionarUsuarioMenu = do
  putStrLn "====================="
  putStrLn "Cadastro de Itens"
  putStrLn "====================="
  putStrLn "1 - Adicionar novo usuario"
  putStrLn "2 - Remover usuario"
  putStrLn "3 - Listar usuarios cadastrados"
  putStrLn "4 - Voltar ao menu principal"
