module Midias where
import Tipos

addItem :: Midia -> [Midia] -> [Midia]
addItem novaMidia lista = novaMidia : lista
  
-- Retorna Nothing se nao achar
buscarItemTalvez :: Codigo -> [Midia] -> Maybe Midia
buscarItemTalvez cod_busca lista =
  -- a função find retorna o primeiro elemento que satisfaz a condição, embrulhado em um Just. Se nenhum satisfaz, retorna Nothing;
  find (\item -> cod item == id_busca) lista

removerItem :: Codigo -> [Midia] -> [Midia]
removerItem cod_remover lista =
  -- mantém apenas os itens que o id é diferente de id_remover;
  filter (\item -> cod item /= id_remover) lista
  
cadastrarMidia :: IO Midia
cadastrarMidia = do
  putStrLn "Qual o tipo de mídia? (1-Livro, 2-Filme, 3-Jogo)"
  putStr "Escolha: "
  tipoStr <- getLine

  criacaoMidia <- case tipoStr of
    "1" -> do
      putStr "Digite o nome do autor: "
      nome <- getLine
      return (AutorLivro nome)
    "2" -> do
      putStr "Digite o nome do diretor: "
      nome <- getLine
      return (AutorFilme nome)
    "3" -> do
      putStr "Digite o nome do criador/estúdio: "
      nome <- getLine
      return (AutorJogo nome)
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      cadastrarMidia

  putStr "Digite o título: "
  tituloMidia <- getLine
  putStr "Digite o ano: "
  anoMidiaStr <- getLine
  putStr "Digite o ID: "
  codMidiaStr <- getLine

  return Midia
    { cod = read codMidiaStr -- read converte codMidiaStr para o tipo especificado;
    , titulo = tituloMidia
    , ano = read anoMidiaStr
    , criacao = criacaoMidia
    }
