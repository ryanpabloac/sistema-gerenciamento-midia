# SISTEMA DE GERENCIAMENTO DE MÍDIAS

## Integrantes
- Ryan Pablo Almeida Costa - 12421BCC050
- Guilherme Silva Cotrim - 12421BCC001
- Tiago Beijo Lemes Monteiro Junior - 12421BCC095
- Matheus Vieira de Assis - 12421BCC040
- João Gabriel Garcia de Oliveira - 12421BCC068

## Processo de instalação
1° Passo - Verificar a versão de seu sistema operacional Ubuntu, pois cada versão exige seus pré-requisitos <br>
2° Passo - Com base na sua versão, consultar a lista abaixo: <br>
**   Versão genérica: Os seguintes pacotes de distribuição são necessários: build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git <br>
   Version >= 20.04 && < 20.10: Os seguintes pacotes de distribuição são necessários: build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git <br>
   Version >= 20.10 && < 23: Os seguintes pacotes de distribuição são necessários: build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git <br>
   Version >= 23: Os seguintes pacotes de distribuição são necessários: build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev git <br>**
3° Passo - Após verificar as dependências da sua versão, por exemplo, versão 23, execute a seguinte linha de comando:
   ```cmd
   sudo apt-get install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev git
   ```
4° Passo - Com as dependências instaladas, basta usar o seguinte comando disponibilizado no site "https://www.haskell.org/ghcup/install/". *Não use o comando como super-usuário:
   ```cmd
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
5° Passo - Siga todos os passos de instalação solicitados. <br>
6° Passo - Clone o repositório pelo git ou baixe o arquivo zip (Nesse caso, deve-se extrair os arquivos para a pasta que desejar). <br>
7° Passo - Acesse a pasta onde está o projeto e abra o terminal de sua preferência, após isso, rodar o projeto usando o seguinte comando:
   ```cmd
   cabal run
   ```
8° Passo - Desfrute de seu sistema de gerenciamento de mídias!
