module Entities.Midias where
import Tipos
import Log.Log
import Data.List
import Data.Char

addItem :: Midia -> [Midia] -> [Midia]
addItem novaMidia lista = novaMidia : lista

-- verifica se o código já existe;
validaCodigoUnico :: Codigo -> [Midia] -> Either String ()
validaCodigoUnico codItem lista =
  if any (\m -> cod m == codItem) lista
  then Left $ "Erro: código \"" ++ show codItem ++ "\" já cadastrado."
  else Right ()

-- valida o ano com base no tipo de mídia;
validaAno :: Ano -> AutorMidia -> IO (Either String ())
validaAno anoMidia criacaoMidia = do
  -- pega a data atual para saber o ano corrente;
  (anoAtual, _, _) <- toGregorian . utctDay <$> getCurrentTime

  let (limiteInferior, tipoStr) = case criacaoMidia of
                                    AutorFilme _ -> (1900, "filmes")
                                    AutorJogo _  -> (1947, "jogos")
                                    AutorLivro _ -> (0, "livros") -- tem livro desde sempre, sei lá;

  if anoMidia >= limiteInferior && anoMidia <= fromIntegral anoAtual
  then return $ Right ()
  else return $ Left $ "Erro: ano \"" ++ show anoMidia ++ "\" inválido para " ++ tipoStr ++
                       ". (Aceito: " ++ show limiteInferior ++ " - " ++ show anoAtual ++ ")"

-- função que combina as validações de mídia;
validaNovaMidia :: Codigo -> Ano -> AutorMidia -> [Midia] -> IO (Either String ())
validaNovaMidia codItem anoMidia criacaoMidia lista = do
  -- primeiro, a validação pura;
  case validaCodigoUnico codItem lista of
    Left erro -> return $ Left erro
    Right () -> do
      -- se passou, executa a validação IO;
      validaAno anoMidia criacaoMidia
  
-- Retorna Nothing se nao achar
buscarItem :: Codigo -> [Midia] -> Maybe Midia
buscarItem cod_busca lista =
  -- a função find retorna o primeiro elemento que satisfaz a condição, embrulhado em um Just. Se nenhum satisfaz, retorna Nothing;
  find (\item -> cod item == cod_busca) lista

removerItem :: Codigo -> [Midia] -> [Midia]
removerItem cod_remover lista =
  -- mantém apenas os itens que o id é diferente de id_remover;
  filter (\item -> cod item /= cod_remover) lista
  
removerItem_IO :: [Midia] -> IO [Midia]
removerItem_IO listaDeMidias = do
  logMessage "INFO" "Sessão de remoção de mídia iniciada."
  putStrLn "-----------"
  putStrLn "Remover item"
  putStrLn "-----------"
  putStr "Informe o código do item a ser removido: "
  codStr <- getLine
  let codigo = read codStr

  -- verifica se o item existe antes de tentar remover;
  case buscarItem codigo listaDeMidias of
    Nothing -> do
      logMessage "WARN" ("Tentativa de remover item com código inexistente: " ++ show codigo) -- log
      putStrLn "ERRO: Item não encontrado."
      return listaDeMidias -- retorna a lista inalterada;
    Just itemParaRemover -> do
      putStrLn $ "Você está prestes a remover: \"" ++ titulo itemParaRemover ++ "\"."
      putStr "Confirma remoção? (S/N): "
      confirmacao <- getLine
      case map toLower confirmacao of
        "s" -> do
          let novaLista = removerItem codigo listaDeMidias
          logMessage "INFO" ("Mídia com código '" ++ show codigo ++ "' foi REMOVIDA.") -- log
          putStrLn "Sucesso! Item removido."
          return novaLista
        _ -> do
          logMessage "INFO" ("Remoção da mídia com código '" ++ show codigo ++ "' foi cancelada.") -- log
          putStrLn "Remoção cancelada."
          return listaDeMidias
  
lerTipoMidia :: IO AutorMidia
lerTipoMidia = do
  putStrLn "Qual o tipo de mídia? (Livro, Filme, Jogo)"
  putStr "Tipo: "
  tipoStr <- getLine

  case map toLower tipoStr of -- toLower para bater o tipo
    "livro" -> do
      logMessage "DEBUG" "Usuário selecionou o tipo 'Livro'."
      putStr "Autor: "
      nome <- getLine
      return (AutorLivro nome)
    "filme" -> do
      logMessage "DEBUG" "Usuário selecionou o tipo 'Filme'."
      putStr "Diretor: "
      nome <- getLine
      return (AutorFilme nome)
    "jogo" -> do
      logMessage "DEBUG" "Usuário selecionou o tipo 'Jogo'."
      putStr "Criador/Estúdio: "
      nome <- getLine
      return (AutorJogo nome)
    _ -> do
      logMessage "WARN" ("Usuário inseriu um tipo de mídia inválido: '" ++ tipoStr ++ "'.") -- log
      putStrLn "Opção inválida. Tente novamente."
      lerTipoMidia

-- função principal;
cadastrarMidia :: [Midia] -> IO Midia
cadastrarMidia = do
  logMessage "INFO" "Sessão de cadastro de nova mídia iniciada." -- log
  -- usa a função auxiliar para pegar os detalhes do tipo e do criador;
  criacaoMidia <- lerTipoMidia

  -- outros dados
  putStr "Digite o título: "
  tituloMidia <- getLine
  putStr "Digite o ano: "
  anoMidiaStr <- getLine
  putStr "Digite o CÓDIGO: "
  codigoMidiaStr <- getLine

  --logMessage "INFO" ("Nova mídia ('" ++ tituloMidia ++ "') criada em memória, aguardando adição à lista principal.") -- log
  
  resultadoValidacao <- validaNovaMidia codigoMidia anoMidia criacaoMidia listaDeMidias
  
  case resultadoValidacao of
    Left erro -> do
      putStrLn erro
      cadastrarMidia listaDeMidias -- tenta novamente;
    Right () -> do
      logMessage "INFO" ("Nova mídia ('" ++ tituloMidia ++ "') validada com sucesso.")
      let novaMidia = Midia
            { cod = codigoMidia
            , titulo = tituloMidia
            , ano = anoMidia
            , criacao = criacaoMidia
            }
      return novaMidia
    
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
      edicaoMidia midiaOriginal

-- função principal que orquestra todo o processo de edição;
editarMidia_IO :: [Midia] -> IO [Midia]
editarMidia_IO listaDeMidias = do
  logMessage "INFO" "Sessão de edição de mídia iniciada."
  putStrLn "-----------"
  putStrLn "Editar item"
  putStrLn "-----------"
  putStr "Informe o código do item: "
  codStr <- getLine
  let codigo = read codStr

  case buscarItem codigo listaDeMidias of
    Nothing -> do
      logMessage "WARN" ("Tentativa de editar item com código inexistente: " ++ show codigo) -- log
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
          logMessage "INFO" ("Mídia com código '" ++ show codigo ++ "' foi alterada.") -- log
          putStrLn "Sucesso! Item atualizado."
          -- AQUI SERIA O LUGAR PARA REGISTRAR O LOG --
          return novaLista
        _ -> do
          logMessage "INFO" ("Edição da mídia com código '" ++ show codigo ++ "' foi cancelada pelo operador.") -- log
          putStrLn "Edição cancelada."
          return listaDeMidias
