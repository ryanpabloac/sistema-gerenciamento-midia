module Entities.Midias where
import Tipos
import Data.List
import Data.Char

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
    
-- função auxiliar para mostrar os dados de uma mídia;
mostrarMidia :: Midia -> IO ()
mostrarMidia item = do
  putStrLn $ "Dados atuais:"
  putStrLn $ "Título: " ++ titulo item
  -- Usamos 'case' para mostrar o criador com o rótulo correto
  case criacao item of
    AutorLivro a -> putStrLn $ "Autor: " ++ a
    AutorFilme d -> putStrLn $ "Diretor: " ++ d
    AutorJogo  c -> putStrLn $ "Criador: " ++ c
  putStrLn $ "Ano: " ++ show (ano item) -- 'show' converte int para string;
  
-- função que gerencia o menu de ediçao e retorna a midia modificada;
edicaoMidia :: Midia -> IO Midia
edicaoMidia midiaOriginal = do
  putStrLn "Escolha campo para editar:"
  putStrLn "1 - Título"
  putStrLn "2 - Criador (Autor/Diretor/etc)"
  putStrLn "3 - Ano"
  putStr "Opção: "
  opcao <- getLine

  case opcao of
    "1" -> do
      putStr "Informe novo título: "
      novoTitulo <- getLine
      return midiaOriginal { titulo = novoTitulo }
    "2" -> do
      putStr "Informe novo criador: "
      novoCriador <- getLine
      -- refaz a estrutura 'criacao' baseada no tipo original;
      let novaCriacao = case criacao midiaOriginal of
                          AutorLivro _ -> AutorLivro novoCriador
                          AutorFilme _ -> AutorFilme novoCriador
                          AutorJogo  _ -> AutorJogo  novoCriador
      return midiaOriginal { criacao = novaCriacao }
    "3" -> do
      putStr "Informe novo ano: "
      novoAnoStr <- getLine
      return midiaOriginal { ano = read novoAnoStr } -- 'read' converte String para Int
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      loopEdicaoMidia midiaOriginal

-- Função principal que orquestra todo o processo de edição
editarMidia_IO :: [Midia] -> IO [Midia]
editarMidia_IO listaDeMidias = do
  putStrLn "-----------"
  putStrLn "Editar item"
  putStrLn "-----------"
  putStr "Informe o código do item: "
  codStr <- getLine
  let codigo = read codStr

  case buscarItemPorCodigo codigo listaDeMidias of
    Nothing -> do
      putStrLn "ERRO: Item não encontrado."
      return listaDeMidias
    Just itemEncontrado -> do
      putStrLn $ "\nItem encontrado: \"" ++ titulo itemEncontrado ++ "\""
      mostrarMidia itemEncontrado
      
      itemModificado <- edicaoMidia itemEncontrado
      
      putStr "Confirma edição? (S/N): "
      confirmacao <- getLine

      case map toLower confirmacao of
        "s" -> do
          let novaLista = map (\m -> if cod m == codigo then itemModificado else m) listaDeMidias
          putStrLn "Sucesso! Item atualizado."
          -- AQUI SERIA O LUGAR PARA REGISTRAR O LOG --
          return novaLista
        _ -> do
          putStrLn "Edição cancelada."
          return listaDeMidias
