module Nim_dumbComputer where

computerTurn :: [Int] -> [Int]
computerTurn (g:gs)
	|g /=0 = [0]++gs
	|otherwise = [g] ++ computerTurn gs 

printBoard :: [Int] -> [String]
printBoard a = map (printLine) a

printLine :: Int -> String
printLine num = concat ["X" | r <- [1..num]]

victory :: Int -> String
victory x
	|x == 1 = "YOU WIN"
	|otherwise = "YOU LOST"

isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove g row num = ((length g > (row -1)) && (g!!(row-1) >= num))

removeMatch :: [Int] -> Int -> Int -> [Int]
removeMatch g row num = (take (row-1) g ++ [(g!!(row-1)-num)] ++ drop (row) g)

playerTurn :: [Int] -> IO ()
playerTurn g = do
	putStrLn "Enter row"
	a <- getLine
	putStrLn "Enter number of sticks"
	b <- getLine
	let row = (read a :: Int)
	let num = (read b :: Int)
	if(isValidMove g row num)
		then do
			let ng = removeMatch g row num
			playGame (ng) 1
	else do
		putStrLn "Not Valid Move"
		playerTurn g
		
playGame :: [Int] -> Int -> IO()
playGame game t = do
	if (all (==0) game)
		then do
			putStrLn $ victory t
	else do
		if (t/= 1)
			then do
				putStrLn "Your turn:"
				mapM_ putStrLn $ (printBoard game)
				putStrLn ""
				playerTurn game
			else do
				putStrLn "Computer turn:"
				mapM_ putStrLn $ printBoard game
				putStrLn ""
				let ng = computerTurn game
				playGame ng 0
				--putStrLn $ victory t
		

main = do
	putStrLn "NIM initiated"
	let game = [4,3,7]
	playGame game 0