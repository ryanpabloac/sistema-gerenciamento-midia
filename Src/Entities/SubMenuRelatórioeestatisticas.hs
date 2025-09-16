import Entities.Tipos
import Entities.Midias
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

import Data.List (sortOn, groupBy)
import Data.Function (on)

data Emprestimo = Emprestimo
  { emprestimoUsuario :: Usuario
  , emprestimoMidia :: Midia
  , dataEmprestimo :: Day
  } deriving (Show, Eq)

-- Representa um item com lista de espera
data ListaEspera = ListaEspera
  { itemEmEspera :: Midia
  , usuariosNaFila :: [Usuario]
  } deriving (Show)

tipoMidia :: Emprestimo -> TipoMidia
tipoMidia emp =
  case criacao (emprestimoMidia emp) of
    AutorLivro _ -> TipoLivro
    AutorFilme _ -> TipoFilme
    AutorJogo  _ -> TipoJogo

EmprestimosAtivos :: [Emprestimo] -> IO ()
EmprestimosAtivos emprestimos = do
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

UsuariosMaisAtivos :: [Emprestimo] -> IO ()
UsuariosMaisAtivos emprestimos = do
  putStrLn "\n--- Usuários mais ativos ---"
  let agruparPorUsuario = group (sortOn matricula (map emprestimoUsuario emprestimos)) -- Extrai os usuários de cada emprestimo, posteriormente os ordena por meio da matricula, após isso os agrupa
  let contagemPorUsuario = [ (head g, length g) | g <- agruparPorUsuario ]             -- Pega o primeiro elemento da lista, e conta quantas vezes aparece, armazenando tais informações em uma tupla
  let usariosDoTopo = take 5 (reverse (sortOn snd contagemPorUsuario))                 -- Ordena as tuplas por meio da quantidade de vezes que aparece, e reverte a ordem da lista, ficando os elementos que mais aparecem nas primeiras posições
  mapM_ imprimirUsuario usariosDoTopo
  where
    imprimirUsuario (usuario, quantidade) =
      putStrLn $ "| Nome: " ++ nome usuario ++ " (" ++ matricula usuario ++ "): realizou " ++ show quantidade ++ " empréstimos"
      
ItensMaisEmprestados :: [Emprestimo] -> IO () -- Funciona de maneira semelhante à função de usuários mais ativos;
ItensMaisEmprestados emprestimos = do
  putStrLn "\n--- Itens mais emprestados ---"
  let agruparPorCodigo = group (sortOn cod (map emprestimoMidia emprestimos))
  let contagemPorCodigo = [ (head g, length g) | g <- agruparPorCodigo]
  let topMidias = take 10 (reverse (sortOn snd contagemPorCodigo))
  mapM_ imprimirMidia topMidias
  where
    imprimirMidia (midia, quantidade) =
      putStrLn $ "| Título: " ++ titulo midia ++ ": " ++ show quantidade ++ " empréstimos"

FrequenciaEmprestimos :: [Emprestimo] -> IO ()
FrequenciaEmprestimos emprestimos = do
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

-- NOTE: Necessidade de passar a lista de emprestimos feita na aba de movimentação;
Relatorio :: [Emprestimo] -> [ListaEspera] -> IO ()
Relatorio listaDeEmprestimos listaDeEspera = do
  putStrLn "\n======================="
  putStrLn "Relatórios e Estatísticas"
  putStrLn "======================="
  putStrLn "1 - Empréstimos ativos (por categoria)"
  putStrLn "2 - Usuários mais ativos"
  putStrLn "3 - Itens mais emprestados"
  putStrLn "4 - Frequência de empréstimos por período"
  putStrLn "5 - Itens com lista de espera"
  putStrLn "6 - Relatório de operações (por usuário / tipo de item)"
  putStrLn "7 - Voltar ao menu principal"
  putStr "Escolha uma opção: "
  
  opcao <- getLine
  loopSubmenuRelatorio listaDeEmprestimos listaDeEspera opcao

loopSubmenuRelatorio :: [Emprestimo] -> [ListaEspera] -> String -> IO ()
loopSubmenuRelatorio listaDeEmprestimos listaDeEspera opcao = do
  case opcao of
    "1" -> do
      EmprestimosAtivos listaDeEmprestimos
      Relatorio listaDeEmprestimos listaDeEspera
    "2" -> do
      UsuariosMaisAtivos listaDeEmprestimos
      Relatorio listaDeEmprestimos listaDeEspera
    "3" -> do
      ItensMaisEmprestados listaDeEmprestimos
      Relatorio listaDeEmprestimos listaDeEspera
    "4" -> do
      FrequenciaEmprestimos listaDeEmprestimos
      Relatorio listaDeEmprestimos listaDeEspera
    "5" -> do
      itensComListaEspera listaDeEspera
      Relatorio listaDeEmprestimos listaDeEspera
    "6" -> do
      
      Relatorio listaDeEmprestimos listaDeEspera
    "7" -> do
      putStrLn "Voltando ao menu principal."
    _ -> do
      putStrLn "Opção inválida. Tente novamente."
      Relatorio listaDeEmprestimos listaDeEspera

































    
