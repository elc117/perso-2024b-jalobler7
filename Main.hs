import System.Random (randomRIO)
	

montyHall :: Bool -> IO Bool
montyHall troca = do
    -- Escolhe aleatoriamente a porta correta
    premio <- randomRIO (1, 3) :: IO Int
    
    -- Jogador faz a primeira escolha
    escolhaJogador <- randomRIO (1, 3) :: IO Int
    
    -- Se o jogador trocar de porta
    let ganhou = if troca
                    then escolhaJogador /= premio  -- Se trocar, ganha se a escolha inicial não for a do prêmio
                    else escolhaJogador == premio  -- Se não trocar, ganha se a escolha inicial for a do prêmio
    return ganhou


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