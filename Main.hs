import System.Random (randomRIO)
import System.IO (hFlush, stdout)


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

jogoMontyHall :: IO ()
jogoMontyHall = do
    premio <- randomRIO (1, 3) :: IO Int
    putStrLn $ "A resposta tá na porta: " ++ show premio
    putStrLn "Digite sua escolha: (Porta 1/2/3)"
    hFlush stdout
    escolhaJogador <- getLine
    let escolha = read escolhaJogador :: Int
    let portas = [1, 2, 3]
    let portasDisponiveis = filter (\x -> x /= escolha && x /= premio) portas
    let portaRevelada = head portasDisponiveis
    putStrLn $ "Não tem nenhum prêmio atrás dessa porta: " ++ show portaRevelada
    putStrLn "Deseja trocar de porta? (sim/nao)"
    hFlush stdout
    resposta <- getLine
    let escolhaFinal = if resposta == "sim"
                    then head (filter (/= escolha) (filter (/= portaRevelada) portas))  
                    else escolha  
    if escolhaFinal == premio
    then putStrLn $ "Parabéns, você ganhou o prêmio! O prêmio estava na porta " ++ show premio
    else putStrLn $ "Que pena, você perdeu! O prêmio estava na porta " ++ show premio

menu :: IO ()
menu = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Simular quantas rodadas quiser"
    putStrLn "2. Jogar o jogo das 3 portas"
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

main :: IO ()
main = do
    putStrLn "Bem-vindo ao simulador de Monty Hall"
    menu
