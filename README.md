# Simulador de Monty Hall
## Nome: João Antônio
## Curso: Sistemas de informação

O problema de Monty Hall é um famoso paradoxo de probabilidade baseado em um jogo de televisão. No jogo, um participante deve escolher uma entre três portas. Atrás de uma delas há um prêmio (geralmente um carro), enquanto as outras duas escondem algo indesejado (como cabras). Após o participante escolher uma porta, o apresentador (que sabe onde está o prêmio) abre uma das outras portas que não contém o prêmio. Ele então oferece ao participante a chance de trocar sua escolha para a porta restante.

O paradoxo está no fato de que, contra a intuição de muitos, trocar de porta aumenta a probabilidade de ganhar de 1/3 para 2/3, pois a porta original tem apenas 1/3 de chance de conter o prêmio, enquanto a outra porta restante concentra 2/3 de chance após a revelação da porta vazia.
O meu código visa possibilitar ao usuário jogar o jogo das 3 portas e também simular quantas vezes quiser o cenário do jogo, de modo que o programa mostre qual foram os resultados trocando ou não de porta.

### Link para vídeo do resultado final:

https://youtu.be/hb3NnHKdrR0


### Fase inicial: 

Comecei fazendo o simulador do jogo, importando a biblioteca de randomização do haskell

~~~
import System.Random (randomRIO)
	
-- Simula uma rodada do Problema de Monty Hall

montyHall :: Bool -> IO Bool
montyHall troca = do
    -- Escolhe aleatoriamente a porta onde está o carro (1, 2 ou 3)
    premio <- randomRIO (1, 3) :: IO Int
    
    -- Jogador faz a primeira escolha
    escolhaJogador <- randomRIO (1, 3) :: IO Int
    
    -- Se o jogador trocar de porta
    let ganhou = if troca
                    then escolhaJogador /= premio  -- Se trocar, ganha se a escolha inicial não for a do prêmio
                    else escolhaJogador == premio  -- Se não trocar, ganha se a escolha inicial for a do prêmio
    return ganhou
  ~~~


### Adcionando algumas funcionalidades:

Após isso adcionei então o que seria a função que definiria quantas vezes seria simulado o jogo e o contador de vitória para cada cenário


~~~
simular :: Int -> IO ()
simular n = do
    -- Simular quando o jogador troca de porta
    vitoriasTroca <- contarVitorias n True
    putStrLn $ "Vitorias trocando de porta: " ++ show vitoriasTroca ++ " de " ++ show n
    putStrLn $ "Probabilidade de ganhar trocando: " ++ show ((fromIntegral vitoriasTroca / fromIntegral n) * 100) ++ "%"
    -- Simular quando o jogador não troca de porta
    vitoriasSemTroca <- contarVitorias n False
    putStrLn $ "Vitorias sem trocar de porta: " ++ show vitoriasSemTroca ++ " de " ++ show n
    putStrLn $ "Probabilidade de ganhar sem trocar: " ++ show ((fromIntegral vitoriasSemTroca / fromIntegral n) * 100) ++ "%"



-- Função auxiliar para contar vitórias


contarVitorias :: Int -> Bool -> IO Int
contarVitorias 0 _ = return 0
contarVitorias n troca = do
    resultado <- montyHall troca
    resto <- contarVitorias (n - 1) troca
    return ((if resultado then 1 else 0) + resto)
~~~

### Primeira versão funcional:

A primeira versão funcional do projeto, apenas simulava quantas vezes o usuário sentisse necessidade mostrando quantas vezes cada estratégia(trocar ou não de porta) venceu. Aí descobri o primeiro problema, a minha lógica do contador de vitórias não tava muito correta aparentemente

~~~
-- Função que executa o loop principal
mainLoop :: IO ()
mainLoop = do
    putStrLn "Quantas rodadas deseja simular? (Digite 0 para sair)"
    n <- readLn :: IO Int
    if n == 0
        then putStrLn "Encerrando a simulação. Até mais!"
        else do
            simular n
            mainLoop  -- Recursivamente chama o loop novamente
main :: IO ()
main = mainLoop

~~~


### Começo da implementação do menu

Então fiz a primeira versão do menu onde já era possível selecionar 2 opções apesar de uma ainda não fazer nada. Ah e descobri um problema da linguagem que existe na leitura das entradas que eu estava tendo rodando o código via terminal. Importei uma biblioteca chamada `System.IO (hFlush, stdout)` que visa terminar com esse problema.


~~~
--função para executar o menu
menu :: IO ()
menu = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Simular quantas rodadas quiser"
    putStrLn "2. Jogar o jogo das 3 portas"
    putStr "Digite sua escolha: "
    hFlush stdout -- função para garantir que o as entradas e as saídas ocorram corretamente
    escolha <- getLine
    case escolha of
        "1" ->do
            putStrLn "Quantas rodadas deseja simular?"
            n <- readLn :: IO Int
            simular n
            menu
        "2"-> do 
            putStrLn "Menu 2 ainda nao funciona"

~~~


### Começo da implantação da parte 2 do menu

Então fiz o código do jogo das 3 portas, ou pelo menos uma versão inicial dele e então incrementei a parte 2 do menu de modo que agora tivesse uma parte também funcional

~~~

jogoMontyHall :: IO()
jogoMontyHall = do
    premio <- randomRIO (1, 3) :: IO Int
    putStrLn $ "A resposta ta na porta: " ++ premio
    putStrLn "Digite sua escolha: (Porta 1/2/3)"
    hFlush stdout
    escolhaJogador <- getLine
    let portas = [1, 2, 3]
    let portasDisponiveis = filter (\x -> x /= escolha && x /= premio) portas
    let portaRevelada = head portasDisponiveis
    putStrLn $ "Não tem nenhum prêmio atrás dessa porta:" ++ show portaRevelada
    putStrLn "Deseja trocar de porta?(Sim/Nao)"
    hFlush stdout
    resposta<- getLine
    let escolhaFinal = if resposta == "sim"
                    then head (filter (/= escolha) (filter (/= portaRevelada) portas))  
                    else escolha  
    if escolhaFinal == premio
    then putStrLn "Parabéns, você ganhou o prêmio!"
    else putStrLn "Que pena, você perdeu!"

~~~
~~~

--função para executar o menu
menu :: IO ()
menu = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Simular quantas rodadas quiser"
    putStrLn "2. Jogar o jogo das 3 portas"
    putStr "Digite sua escolha: "
    hFlush stdout -- função para garantir que o as entradas e as saídas ocorram corretamente
    escolha <- getLine
    case escolha of
        "1" ->do
            putStrLn "Quantas rodadas deseja simular?"
            n <- readLn :: IO Int
            simular n
            menu
        "2"-> do 
            putStrLn "Menu 2 ainda nao funciona"
            putStrLn "O jogo das 3 portas de Monty Hall envolve um participante escolhendo uma entre três portas, onde apenas uma esconde um prêmio e as outras duas não possuem prêmios. Após a escolha inicial, o apresentador revela uma das portas não escolhidas onde não há prêmio e oferece a chance de trocar de porta."
            jogoMontyHall
            menu
main :: IO ()
main = do
    putStrLn "Bem vindo so simulador de Monty Hall"
~~~

  
### Mudanças simples:

Aqui fiz algumas mudanças simples no código apenas pela interatividade e para testar o funcionamento correto do código
![image](https://github.com/user-attachments/assets/7cc0e4c2-8b4c-4693-a847-b459d5a6367e)

### Função ContaVitorias

A função que mais me deu dor de cabeça até aqui, a ideia era fazer uma função auxiliar que contasse as vitórias em cada metódo porém a lógica não está funcionando

~~~
-- Função auxiliar para contar vitórias
contarVitorias :: Int -> Bool -> IO Int
contarVitorias 0 _ = return 0
contarVitorias n troca = do
    resultado <- montyHall troca
    resto <- contarVitorias (n - 1) troca
    return ((if resultado then 1 else 0) + resto)

-- Função para simular as rodadas e gerar estatísticas
simular :: Int -> IO ()
simular n = do
    -- Simular quando o jogador troca de porta
    vitoriasTroca <- contarVitorias n True
    putStrLn $ "Vitórias trocando de porta: " ++ show vitoriasTroca ++ " de " ++ show n
    putStrLn $ "Probabilidade de ganhar trocando: " ++ show ((fromIntegral vitoriasTroca / fromIntegral n) * 100) ++ "%"

    -- Simular quando o jogador não troca de porta
    vitoriasSemTroca <- contarVitorias n False
    putStrLn $ "Vitórias sem trocar de porta: " ++ show vitoriasSemTroca ++ " de " ++ show n
    putStrLn $ "Probabilidade de ganhar sem trocar: " ++ show ((fromIntegral vitoriasSemTroca / fromIntegral n) * 100) ++ "%"

    putStrLn $ "Foram jogados " ++ show n ++ " jogos"
~~~
A lógica que estou tentando implementar na função contarVitorias é a seguinte: ela recebe dois argumentos, um número inteiro N que representa a quantidade de partidas, e um booleano que indica o comportamento do jogador. Se o valor do booleano for True, a função contará as vitórias quando o jogador troca de porta; se for False, contará as vitórias quando o jogador não troca de porta. O objetivo é que a função incremente corretamente o contador de vitórias para cada um dos casos. No entanto, algo está errado na lógica, pois a contagem das vitórias não está sendo feita corretamente.


![image](https://github.com/user-attachments/assets/d3832cc1-1d53-48a4-aa8c-df7528da2354)


### Correção do contador de vitórias

Dado que eu não consegui fazer a função contaVitórias funcionar decidi trocar o metódo de soma das vitórias, fiz então o uso de função recursiva a função 'simularRodadas' recebe 3 parâmetros de tipo inteiro são eles, o caso base(que vai ser sempre zero), as vitórias com troca e as vitórias sem troca. Ele mesmo incrementa a variavél em que está guardada o número de vitórias e decrementa o número de rodadas restantes até que se chegue no caso base que é 0. 

~~~

montyHall :: IO (Bool, Bool)
montyHall = do

    premio <- randomRIO (1, 3) :: IO Int
    
    escolhaJogador <- randomRIO (1, 3) :: IO Int
    
    let ganhouSemTroca = escolhaJogador == premio  
    let ganhouComTroca = escolhaJogador /= premio  
    return (ganhouSemTroca, ganhouComTroca)

simular :: Int -> IO ()
simular n = simularRodadas n 0 0
  where

    simularRodadas :: Int -> Int -> Int -> IO ()
    simularRodadas 0 vitoriasSemTroca vitoriasComTroca = do
      
        putStrLn $ "Vitórias trocando de porta: " ++ show vitoriasComTroca ++ " de " ++ show n
        putStrLn $ "Probabilidade de ganhar trocando: " ++ show ((fromIntegral vitoriasComTroca / fromIntegral n) * 100) ++ "%"
        putStrLn $ "Vitórias sem trocar de porta: " ++ show vitoriasSemTroca ++ " de " ++ show n
        putStrLn $ "Probabilidade de ganhar sem trocar: " ++ show ((fromIntegral vitoriasSemTroca / fromIntegral n) * 100) ++ "%"
        putStrLn $ "Foram jogados " ++ show n ++ " jogos"
    simularRodadas rodadasRestantes vitoriasSemTroca vitoriasComTroca = do

        (ganhouSemTroca, ganhouComTroca) <- montyHall
        
        let novasVitoriasSemTroca = if ganhouSemTroca then vitoriasSemTroca + 1 else vitoriasSemTroca
        let novasVitoriasComTroca = if ganhouComTroca then vitoriasComTroca + 1 else vitoriasComTroca
        
        simularRodadas (rodadasRestantes - 1) novasVitoriasSemTroca novasVitoriasComTroca
~~~

### Implementação da parte 3(Menu de ajuda)

Por sugestão de amigos, me pareceu boa ideia inserir um menu de ajuda que explicasse o jogo e o paradoxo de Monty Hall. De modo bem simples apenas para dar uma visão geral para quem não conhece o jogo. E também implementei um botão de saída.

~~~

 putStrLn "3. Como funciopna o jogo das 3 portas"
    putStrLn "0. Digite 0 para sair"
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            putStrLn "Quantas rodadas deseja simular?"
            n <- readLn :: IO Int
            simular n
            menu
        "2" -> do 
            jogoMontyHall
            menu
        "3" -> do 
            putStrLn "O jogo das 3 portas de Monty Hall envolve um participante escolhendo uma entre três portas, onde apenas uma esconde um prêmio e as outras duas não possuem prêmios. Após a escolha inicial, o apresentador revela uma das portas não escolhidas onde não há prêmio e oferece a chance de trocar de porta."
            putStrLn "Onde está o paradoxo?"
            putStrLn "O paradoxo está no fato de que, contra a intuição de muitos, trocar de porta aumenta a probabilidade de ganhar de 1/3 para 2/3, pois a porta original tem apenas 1/3 de chance de conter o prêmio, enquanto a outra porta restante concentra 2/3 de chance após a revelação da porta vazia. O meu código visa possibilitar ao usuário jogar o jogo das 3 portas e também simular quantas vezes quiser o cenário do jogo, de modo que o programa mostre qual foram os resultados trocando ou não de porta."
            menu
        "0" -> do 
            putStrLn "Obrigado por jogar"

~~~

### Implementação de verificação se a resposta foi sim ou não

Um verificador para garantir que o jogo só funcione quando o usuário coloque "sim" ou "nao" foi adcionado

~~~

jogoMontyHall :: IO ()
jogoMontyHall = do
    premio <- randomRIO (1, 3) :: IO Int
    --putStrLn $ "A resposta tá na porta: " ++ show premio
    putStrLn "Digite sua escolha: (Porta 1/2/3)"
    hFlush stdout
    escolhaJogador <- getLine
    let escolha = read escolhaJogador :: Int
    let portas = [1, 2, 3]
    let portasDisponiveis = filter (\x -> x /= escolha && x /= premio) portas
    let portaRevelada = head portasDisponiveis
    putStrLn $ "Na porta " ++ show portaRevelada ++ " não tem nenhum prêmio"
    putStrLn "Deseja trocar de porta? (sim/nao)"
    hFlush stdout
    resposta <- getLine
    if resposta /= "sim" && resposta /= "nao" 
        then do
            putStrLn "A resposta deve ser 'sim' ou 'nao'" 
            jogoMontyHall
    else do
        let escolhaFinal = if resposta == "sim"
                    then head (filter (/= escolha) (filter (/= portaRevelada) portas))  
                    else escolha  
        if escolhaFinal == premio
        then putStrLn $ "Parabéns, você ganhou o prêmio! O prêmio estava na porta " ++ show premio
        else putStrLn $ "Que pena, você perdeu! O prêmio estava na porta " ++ show premio

~~~

### Considerações finais 

Trabalhar com paradoxos e temas desse tipo foi uma experiência que me agradou bastante, especialmente ao ter a liberdade de escolher o tema para o projeto. Enfrentei algumas dificuldades ao lidar com uma linguagem nova, o que sempre traz o desafio de compreender a sintaxe e os conceitos fundamentais. Um dos maiores desafios que encontrei foi na parte lógica do programa, particularmente ao desenvolver a função de contar vitórias, que exigiu um esforço maior para ser implementada corretamente.


#### Fontes

* https://stackoverflow.com/search?q=empty+do+haskell&s=59d90386-fae2-4bd0-98ca-999a3a0e26b6&s=f7e85cea-d053-44a2-8b9d-5b1c6dda4728
* https://pt.wikibooks.org/wiki/Haskell/Casamento_de_padr%C3%B5es,_if_e_let
* https://haskell.tailorfontela.com.br/syntax-in-functions
* https://gizmodo.uol.com.br/paradoxo-monty-hall/






