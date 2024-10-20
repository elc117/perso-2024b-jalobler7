import System.Random (randomRIO)
import System.IO (hFlush, stdout)
	

montyHall :: Bool -> IO Bool
montyHall troca = do
    -- Escolhe aleatoriamente a porta correta
    premio <- randomRIO (1, 3) :: IO Int
    
    escolhaJogador <- randomRIO (1, 3) :: IO Int
    
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
            putStrLn "O jogo das 3 portas de Monty Hall envolve um participante escolhendo uma entre três portas, onde apenas uma esconde um prêmio e as outras duas não possuem prêmios. Após a escolha inicial, o apresentador revela uma das portas não escolhidas onde não há prêmio e oferece a chance de trocar de porta."
            jogoMontyHall
            menu
main :: IO ()
main = do
    putStrLn "Bem vindo so simulador de Monty Hall"
    menu