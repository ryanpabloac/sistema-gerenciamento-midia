module Entities.Midias where
import Tipos
import Data.List
import Data.Char (toLower)

addItem :: Midia -> [Midia] -> [Midia]
addItem novaMidia lista = novaMidia : lista
  
-- Retorna Nothing se nao achar
buscarItemTalvez :: Codigo -> [Midia] -> Maybe Midia
buscarItemTalvez cod_busca lista =
  -- a função find retorna o primeiro elemento que satisfaz a condição, embrulhado em um Just. Se nenhum satisfaz, retorna Nothing;
  find (\item -> cod item == cod_busca) lista

removerItem :: Codigo -> [Midia] -> [Midia]
removerItem cod_remover lista =
  -- mantém apenas os itens que o id é diferente de id_remover;
  filter (\item -> cod item /= cod_remover) lista
  
lerTipoMidia :: IO AutorMidia
lerTipoMidia = do
  putStrLn "Qual o tipo de mídia? (Livro, Filme, Jogo)"
  putStr "Tipo: "
  tipoStr <- getLine

  case map toLower tipoStr of -- toLower para bater o tipo
    "livro" -> do
      putStr "Autor: "
      nome <- getLine
      return (AutorLivro nome)
    "filme" -> do
      putStr "Diretor: "
      nome <- getLine
      return (AutorFilme nome)
    "jogo" -> do
      putStr "Criador/Estúdio: "
      nome <- getLine
      return (AutorJogo nome)
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      lerTipoMidia

-- função principal;
cadastrarMidia :: IO Midia
cadastrarMidia = do
  -- usa a função auxiliar para pegar os detalhes do tipo e do criador;
  criacaoMidia <- lerTipoMidia

  -- outros dados
  putStr "Digite o título: "
  tituloMidia <- getLine
  putStr "Digite o ano: "
  anoMidiaStr <- getLine
  putStr "Digite o CÓDIGO: "
  codigoMidiaStr <- getLine

  return Midia
    { cod = read codigoMidiaStr
    , titulo = tituloMidia
    , ano = read anoMidiaStr
    , criacao = criacaoMidia
    }
