module Services.Arquivos where

import Entities.Tipos
import Log.Log
import Data.List
import Text.Read (readMaybe)
--import Data.List.Split (splitOn) -- se der para carregar esse modulo, pode remover 
import Data.Char (toLower)
import Control.Monad (forM, when) -- troca o foldr por forM, permitindo executar uma ação de IO (logMessage) para cada linha do arquivo, algo que o foldr (puro) não permite;
import Data.Maybe (catMaybes) -- filtrar resultados inválidos

-- splitPor divide uma String por um caractere delimitador.
-- exemplo: splitPor ',' "a,b,c" == ["a", "b", "c"]
splitPor :: Char -> String -> [String]
splitPor _ "" = []
splitPor delimitador str =
  let
    (primeiraParte, resto) = break (== delimitador) str
  in
    primeiraParte : case resto of
      "" -> []
      (_:xs) -> splitPor delimitador xs
      
-- função de parse (analisa as linhas)
parseUsuario :: String -> Either String Usuario
parseUsuario linha =
  case splitPor ',' linha of
    [n, m, e] -> Right (Usuario n m e)
    _ -> Left $ "Linha de usuário mal formatada: " ++ linha

-- função principal de carregamento;
carregarUsuarios :: FilePath -> IO [Usuario]
carregarUsuarios caminho = do
  logMessage "INFO" ("Iniciando carregamento do arquivo de usuários: '" ++ caminho ++ "'.") -- log
  conteudo <- readFile caminho
  let linhas = tail (lines conteudo) -- pula o cabeçalho;
  logMessage "DEBUG" (show (length linhas) ++ " linhas de dados encontradas em '" ++ caminho ++ "'.") -- log
  
  -- usa o forM para processar cada linha e permitir IO
  maybeUsuarios <- forM linhas $ \linha -> do
    case parseUsuario linha of
      Left erro -> do
        logMessage "WARN" ("Falha ao parsear linha de usuário. Erro: " ++ erro) -- log
        return Nothing -- Marcamos a linha como inválida
      Right user -> return (Just user) -- Marcamos a linha como válida

  let usuariosCarregados = catMaybes maybeUsuarios -- 'catMaybes' filtra e extrai os usuários válidos;
  let numErros = length linhas - length usuariosCarregados
  
  logMessage "INFO" ("Carregamento de '" ++ caminho ++ "' concluído. " ++ show (length usuariosCarregados) ++ " usuários carregados. " ++ show numErros ++ " linhas com erro.")
  
  -- imprime os erros no console para o operador, além de estarem no log;
  when (numErros > 0) $
    putStrLn $ show numErros ++ " linhas no arquivo de usuários não puderam ser lidas e foram ignoradas. Verifique o arquivo de log para detalhes."
    
  return usuariosCarregados

-- mesma coisa para midias;
parseMidia :: String -> Either String Midia
parseMidia linha =
  case splitPor ',' linha of
    [codStr, tipo, tituloMidia, anoStr, criadorMidia] ->
      case (readMaybe codStr, readMaybe anoStr) of
        (Just codigoMidia, Just anoCriacao) ->
          let criacao = case map toLower tipo of
                          "livro" -> AutorLivro criadorMidia
                          "filme" -> AutorFilme criadorMidia
                          "jogo"  -> AutorJogo criadorMidia
                          _       -> AutorLivro "INDEFINIDO"
          in Right $ Midia codigoMidia tituloMidia anoCriacao criacao
        _ -> Left $ "Erro de conversão de número na linha: " ++ linha
    _ -> Left $ "Linha de mídia mal formatada: " ++ linha

-- Função principal de carregamento com logs
carregarMidias :: FilePath -> IO [Midia]
carregarMidias caminho = do
  logMessage "INFO" ("Iniciando carregamento do arquivo de mídias: '" ++ caminho ++ "'.") -- log
  conteudo <- readFile caminho
  let linhas = tail (lines conteudo)
  logMessage "DEBUG" (show (length linhas) ++ " linhas de dados encontradas em '" ++ caminho ++ "'.") -- log

  maybeMidias <- forM linhas $ \linha -> do
    case parseMidia linha of
      Left erro -> do
        logMessage "WARN" ("Falha ao parsear linha de mídia. Erro: " ++ erro)
        return Nothing
      Right midia -> return (Just midia)

  let midiasCarregadas = catMaybes maybeMidias
  let numErros = length linhas - length midiasCarregadas
  
  logMessage "INFO" ("Carregamento de '" ++ caminho ++ "' concluído. " ++ show (length midiasCarregadas) ++ " mídias carregadas. " ++ show numErros ++ " linhas com erro.")

  when (numErros > 0) $
    putStrLn $ show numErros ++ " linhas no arquivo de mídias não puderam ser lidas e foram ignoradas. Verifique o arquivo de log para detalhes."

  return midiasCarregadas
  
-- FUNÇÕES PARA SALVAR ALTERAÇÕES E TER A PERSISTENCIA ENTRE EXECUÇÕES;

-- converte um único usuario para uma linha de CSV;
formatarUsuario :: Usuario -> String
formatarUsuario u = intercalate "," [nome u, matricula u, email u]

-- salva uma lista de usuarios no arquivo (sobrescrevendo o antigo);
salvarUsuarios :: FilePath -> [Usuario] -> IO ()
salvarUsuarios caminho usuarios = do
  let cabecalho = "Nome,Matricula,Email"
  -- converte cada usuário para uma linha de CSV
  let linhas = map formatarUsuario usuarios
  -- junta o cabeçalho com as linhas, separando com quebras de linha;
  let conteudoCompleto = unlines (cabecalho : linhas)
  
  -- escreve o conteudo final no arquivo
  writeFile caminho conteudoCompleto
  logMessage "INFO" ("Dados de " ++ show (length usuarios) ++ " usuários salvos em '" ++ caminho ++ "'.")

-- converte uma única mídia;
formatarMidia :: Midia -> String
formatarMidia m =
  let
    -- 'case' para determinar o tipo e o criador corretos;
    (tipoStr, criadorStr) = case criacao m of
                              AutorLivro a -> ("Livro", a)
                              AutorFilme d -> ("Filme", d)
                              AutorJogo  c -> ("Jogo", c)
  in
    -- junta todas as partes com vírgulas
    intercalate "," [show (cod m), tipoStr, titulo m, show (ano m), criadorStr]

-- salva uma lista de mídias no arquivo;
salvarMidias :: FilePath -> [Midia] -> IO ()
salvarMidias caminho midias = do
  let cabecalho = "Codigo,Tipo,Titulo,Ano,Criador"
  let linhas = map formatarMidia midias
  let conteudoCompleto = unlines (cabecalho : linhas)
  
  writeFile caminho conteudoCompleto
  logMessage "INFO" ("Dados de " ++ show (length midias) ++ " mídias salvas em '" ++ caminho ++ "'.")

parseEmprestimo :: [Usuario] -> [Midia] -> String -> Maybe Emprestimo
parseEmprestimo usuarios midias linha = do
  [matStr, codStr, dataStr] <- Just (splitPor ',' linha)
  codigo <- readMaybe codStr
  dataEmp <- readMaybe dataStr

  usuario <- find (\u -> matricula u == matStr) usuarios
  midia <- find (\m -> cod m == codigo) midias

  return $ Emprestimo usuario midia dataEmp
 
 
carregarEmprestimos :: FilePath -> [Usuario] -> [Midia] -> IO [Emprestimo]
carregarEmprestimos caminho usuarios midias = do
    conteudo <- readFile caminho
    let linhas = tail (lines conteudo)
    let maybeEmprestimos = catMaybes $ map (parseEmprestimo usuarios midias) linhas
    let numErros = length linhas - length maybeEmprestimos
    
    logMessage "INFO" ("Carregamento de '" ++ caminho ++ "' concluído. " ++ show (length maybeEmprestimos) ++ " empréstimos carregados. " ++ show numErros ++ " linhas com erro.")

    when (numErros > 0) $
      putStrLn $ show numErros ++ " linhas no arquivo de empréstimos não puderam ser lidas e foram ignoradas. Verifique o arquivo de log para detalhes."
    
    return maybeEmprestimos
 
 
formatarEmprestimo :: Emprestimo -> String
formatarEmprestimo e =
  intercalate "," [matricula (emprestimoUsuario e), show (cod (emprestimoMidia e)), show (dataEmprestimo e)]
  
 
 
salvarEmprestimos :: FilePath -> [Emprestimo] -> IO ()
salvarEmprestimos caminho emprestimos = do
  let cabecalho = "Matricula,Codigo,Data"
  let linhas = map formatarEmprestimo emprestimos
  let conteudoCompleto = unlines (cabecalho : linhas)
  
  writeFile caminho conteudoCompleto
  logMessage "INFO" ("Dados de " ++ show (length emprestimos) ++ " empréstimos salvos em '" ++ caminho ++ "'.")
