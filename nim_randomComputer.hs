module Nim_randomComputer where

import System.Random

randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)
 
randomizer s = head (randomList s :: [Int])

converter :: IO Int ->IO String
converter num = do
	n <- num
	return ("" ++ (show n))

--computerTurn :: [Int] -> [Int]
computerTurn g= do
	let row = randomRIO (1, (length g))
	let st = randomRIO (1, 100)
	a <- converter row
	b <- converter st
	if isValidMove g (read a :: Int) (read b :: Int)
		then do
			let t = removeMatch g (read a :: Int) (read b :: Int)
			playGame (t) 0
	else do
		computerTurn g

printBoard :: [Int] -> [String]
printBoard a = map (printLine) a

printLine :: Int -> String
printLine num = concat ["X" | r <- [1..num]]

victory :: Int -> String
victory x
	|x == 1 = "YOU WIN"
	|otherwise = "YOU LOST"

--isValidMove :: [Int] -> Int -> Int -> Bool
isValidMove g row num = (num > 0 && (length g > (row -1)) && (g!!(row-1) >= num))

--removeMatch :: [Int] -> Int -> Int -> [Int]
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
				mapM_ putStrLn $ printBoard game
				playerTurn game
			else do
				computerTurn game
				--putStrLn $ victory t
		

main = do
	putStrLn "NIM initiated"
	let game = [4,3,7]
	playGame game 0