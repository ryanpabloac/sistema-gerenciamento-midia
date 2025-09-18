{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_sistema_gerenciamento_midia (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "sistema_gerenciamento_midia"
version :: Version
version = Version [0,0,1,0] []

synopsis :: String
synopsis = "Sistema de gerenciamento de m\237dias"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
