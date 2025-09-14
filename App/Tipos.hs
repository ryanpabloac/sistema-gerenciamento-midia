module Tipos where
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.List
import Data.Char

-- SINÔNIMOS DE TIPO
-- usuarios
type Nome = String
type Matricula = String
type Email = String

-- midia
type Titulo = String
type Autor = String
type Diretor = String
type Criador = String
type Ano = Int
type Codigo = Int

-- TIPOS DE DADOS

-- Estrutura para Usuário (permanece a mesma)
data Usuario = Usuario
  { nome      :: Nome
  , matricula :: Matricula
  , email     :: Email
  }
  deriving (Show, Eq)

-- Para classificar a mídia
data TipoMidia = TipoLivro | TipoFilme | TipoJogo
  deriving (Show, Eq)

-- Estrutura para guardar os criadores, ja que sao todos do mesmo tipo
data AutorMidia = AutorLivro { autor :: Autor } | AutorFilme { diretor :: Diretor } | AutorJogo  { criador :: Criador }
  deriving (Show, Eq)

-- estrutura para qualquer midia
data Midia = Midia
  { cod     :: Codigo
  , titulo  :: Titulo
  , ano     :: Ano
  , criacao :: AutorMidia
  }
  deriving (Show, Eq)
