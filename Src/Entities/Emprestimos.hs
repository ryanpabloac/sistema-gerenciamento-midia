module Entities.Emprestimos where

import Data.List (sortOn, groupBy)
import Data.Function (on)

tipoMidia :: Emprestimo -> TipoMidia
tipoMidia emp =
  case criacao (emprestimoMidia emp) of
    AutorLivro _ -> TipoLivro
    AutorFilme _ -> TipoFilme
    AutorJogo  _ -> TipoJogo
    
emprestimosAtivos :: [Emprestimo] -> IO ()
emprestimosAtivos emprestimos = do
  putStrLn "\n--- Empréstimos Ativos (por Categoria) ---"
  let agruparPorTipo = groupBy ((==) `on` tipoMidia) (sortOn tipoMidia emprestimos) -- ordenação com base no tipo de mídia, posteriorente à ordenação é agrupado por "groupBy";
  mapM_ imprimirGrupos agruparPorTipo
  where
    imprimirGrupos xs = do
      let tipo = tipoMidia (head xs)                                                -- Captura o tipo de midia do primeiro argumento do grupo;                                               
      case tipo of 
        TipoLivro -> putStrLn "Empréstimos ativos - Livros"
        TipoFilme -> putStrLn "Empréstimos ativos - Filmes"
        TipoJogo  -> putStrLn "Empréstimos ativos - Jogos"
      putStrLn "----------------------------"
      mapM_ detalhesEmprestimo xs
    detalhesEmprestimo e =
      putStrLn $ "Código: " ++ show (cod (emprestimoMidia e)) ++
                 " | Título: " ++ titulo (emprestimoMidia e) ++ 
                 " | Usuário: " ++ nome (emprestimoUsuario e) ++
                 " | Data: " ++ show (dataEmprestimo e)

usuariosMaisAtivos :: [Emprestimo] -> IO ()
usuariosMaisAtivos emprestimos = do
  putStrLn "\n--- Usuários mais ativos ---"
  let agruparPorUsuario = group (sortOn matricula (map emprestimoUsuario emprestimos)) -- Extrai os usuários de cada emprestimo, posteriormente os ordena por meio da matricula, após isso os agrupa
  let contagemPorUsuario = [ (head g, length g) | g <- agruparPorUsuario ]             -- Pega o primeiro elemento da lista, e conta quantas vezes aparece, armazenando tais informações em uma tupla
  let usariosDoTopo = take 5 (reverse (sortOn snd contagemPorUsuario))                 -- Ordena as tuplas por meio da quantidade de vezes que aparece, e reverte a ordem da lista, ficando os elementos que mais aparecem nas primeiras posições
  mapM_ imprimirUsuario usariosDoTopo
  where
    imprimirUsuario (usuario, quantidade) =
      putStrLn $ "| Nome: " ++ nome usuario ++ " (" ++ matricula usuario ++ "): realizou " ++ show quantidade ++ " empréstimos"
      
itensMaisEmprestados :: [Emprestimo] -> IO () -- Funciona de maneira semelhante à função de usuários mais ativos;
itensMaisEmprestados emprestimos = do
  putStrLn "\n--- Itens mais emprestados ---"
  let agruparPorCodigo = group (sortOn cod (map emprestimoMidia emprestimos))
  let contagemPorCodigo = [ (head g, length g) | g <- agruparPorCodigo]
  let topMidias = take 10 (reverse (sortOn snd contagemPorCodigo))
  mapM_ imprimirMidia topMidias
  where
    imprimirMidia (midia, quantidade) =
      putStrLn $ "| Título: " ++ titulo midia ++ ": " ++ show quantidade ++ " empréstimos"

frequenciaEmprestimos :: [Emprestimo] -> IO ()
frequenciaEmprestimos emprestimos = do
  putStrLn "\n--- Frequência de Empréstimos por Período ---"
  let agrupamentodatas = group . sort $ map dataEmprestimo emprestimos
  mapM_ imprimirfrequencia agrupamentodatas
  where 
    imprimirfrequencia grupo = do
    let dataEmp    = head grupo
    let quantidade =  length grupo
    putStrLn $ "| Data: " ++ show dataEmp ++ ": " ++ show quantidade ++ " empréstimos"

itensComListaEspera :: [ListaEspera] -> IO ()
itensComListaEspera xs = do
  putStrLn "\n--- Itens com lista de espera ---"
  if null xs
    then putStrLn "Nenhum Item com lista de espera no momento"
    else mapM_ imprimirListaEspera xs
  where
    imprimirListaEspera xs = do 
      let tituloMidia = titulo (itemEmEspera xs)
      let numUsuarios = length (usuariosNaFila xs)
      putStrLn $ "| Título: " ++ tituloMidia ++ ": " ++ show numUsuarios ++ " pessoas na fila"

relatorioOperacoes :: [Emprestimo] -> IO ()
relatorioOperacoes emprestimos = do
  putStrLn "\n--- Relatório de Operações ---"
  putStrLn "Digite o nome do usuário ou o tipo de mídia (Livro, Filme, Jogo):"
  putStr "Critério de busca: "
  criterio <- getLine
  putStrln $ "\nResultados para: \"" ++ criterio ++ "\""
  let resultados = filter (filtraPorCriterio criterio) emprestimos
  if null resultados
    then putStrLn "Nenhuma operação encontrada para este critério."
    else mapM_ imprimirDetalhesOperacao resultados
  where
    filtraPorCriterio criterio e =
      (nome (emprestimoUsuario e) == criterio) ||
      (show (tipoMidia e) == criterio)
    imprimirDetalhesOperacao e =
      putStrLn $ "| Título : " ++ titulo (emprestimoMidia e) ++
                 " emprestado por " ++ nome (emprestimoUsuario e) ++
                 " em " ++ show (dataEmprestimo e)
