module NQueens where
import Control.Monad
import Data.List
import System.IO  
import System.Directory  

calcSolution :: Int -> [[Int]]
calcSolution n = map fst $ foldM addQueen ([],[1..n]) [1..n]  where 
 
  -- adds a queen to the board
  addQueen (y,d) _ = [(x:y, delete x d) | x <- d, isSafe x]  where
 
    -- "safe x" tests whether a queen at column x is safe from previous queens
    isSafe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]
    -- returns whether a queen is safe from other queens

-- prints solutions
printSol y = do
  mapM_ (\x -> putStrLn [if z == x then 'Q' else 'X' | z <- [1..(length y)]]) y
  putStrLn ""

--prompts you for a number
prompt x = do
  putStr x
  getLine

nQueens = do
	number <- prompt "Please input a number: "
	let size = (read number::Int)
	if size == 2
		then putStrLn "No solution"
	else  
			(if size == 3
			then putStrLn "No solution"
			else 
			 mapM_ printSol $ calcSolution size)


nQueensNumSoln = do
  number <- prompt "Please input a number: "
  let size = (read number::Int)
  if size == 2
		then putStrLn "No solution"
	else  
			(if size == 3
			then putStrLn "No solution"
			else 
			 putStrLn (show (length (calcSolution size))))

