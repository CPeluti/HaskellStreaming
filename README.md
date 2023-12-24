[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/hiWa6Cqc)

# Haspotifaskell
## API de streaming de música escrita em Haskell
## Principais features
1. Streaming de mp3.
2. Interface web para reprodução de músicas
3. Armazenamendo de meta-dados usando sqlite
4. Escrito em Haskell -- inserir emoji de fire aqui --

## Instalação
### Dependencias
- Todas as bibliotecas necessárias para sqlite3
- Stack
- Git

### Como instalar
Primeiramente clone o repositório

`$ git clone https://github.com/Prof-Edil/projeto-final-grupo-2.git`

Em seguida, instale as dependencias do projeto
`$ cd projeto-final-grupo-2 && stack install`

### Como executar

`$ stack run`

## Como cadastrar uma musica
Basta acessar o endereço do servidor na rota `uploadPage`
IMPORTANTE: O campo length é o tamanho da música em segundos!

## Como ouvir uma musica
### Requisitos
1. Estar com o servidor rodando
2. Possuir um dispositivo emissor de audio
3. Ter uma música cadastrada no sistema
### Como fazer
Basta clicar no botão de play da música desejada, disponível na rota `/`

## Contribuições
@CaioPeluti - Streaming, banco de dados e API
@AnaJordao - Streaming e banco de dados
@luccahuguet - HTMX e API
