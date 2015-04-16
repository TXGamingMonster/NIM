module NIM where

printBoard :: [Int] -> [String]
printBoard a = map (printLine) a

printLine :: Int -> String
printLine num = concat ["|" | r <- [0..num]]

isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove g row num = ((length g > row -1) && (g!!(row-1) > num))

removeMatch :: [Int] -> Int -> Int -> [Int]
removeMatch g row num = (take (row-2) g ++ [(g!!(row-1)-num)] ++ drop (row) g)

--playerTurn :: [Int] -> IO [Int]-> [Int]
playerTurn g = do
	putStrLn "Enter row and number of sticks"
	row <- getLine
	num <- getLine
	if(isValidMove g (read row :: Int) (read num :: Int))
		then do
			let ng = removeMatch g (read row :: Int) (read num :: Int)
			playGame $ ng 1
	else do
		putStrLn "Not Valid Move"
		playerTurn g
		
playGame :: [Int] -> Int -> IO()
playGame game t = do
	if (all (==0) game)
		then do
			putStrLn "YOU WIN"
	else do
		mapM_ putStrLn $ printBoard game
		playerTurn game
		putStrLn "Game Over"
		

main = do
	putStrLn "NIM initiated"
	let game = [4,3,7]
	playGame game 0