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



  
