# Simulador de Monty Hall
## Nome: João Antônio
## Curso: Sistemas de informação

O problema de Monty Hall é um famoso paradoxo de probabilidade baseado em um jogo de televisão. No jogo, um participante deve escolher uma entre três portas. Atrás de uma delas há um prêmio (geralmente um carro), enquanto as outras duas escondem algo indesejado (como cabras). Após o participante escolher uma porta, o apresentador (que sabe onde está o prêmio) abre uma das outras portas que não contém o prêmio. Ele então oferece ao participante a chance de trocar sua escolha para a porta restante.

O paradoxo está no fato de que, contra a intuição de muitos, trocar de porta aumenta a probabilidade de ganhar de 1/3 para 2/3, pois a porta original tem apenas 1/3 de chance de conter o prêmio, enquanto a outra porta restante concentra 2/3 de chance após a revelação da porta vazia.
O meu código visa possibilitar ao usuário jogar o jogo das 3 portas e também simular quantas vezes quiser o cenário do jogo, de modo que o programa mostre qual foram os resultados trocando ou não de porta.


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


  
