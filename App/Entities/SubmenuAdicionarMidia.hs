module SubmenuAdicionarMidia where
import Tipos

adicionarMidiaMenu :: IO ()
adicionarMidiaMenu = do
  putStrLn "====================="
  putStrLn "Cadastro de Itens"
  putStrLn "====================="
  putStrLn "1 - Adicionar novo item"
  putStrLn "2 - Remover item"
  putStrLn "3 - Listar itens cadastrados"
  putStrLn "4 - Voltar ao menu principal"
