module NQueens where


nQueens size
	|size == 2 = "No solution"
	|size == 3 = "No solution"
	|otherwise = length $ nQueensNumSoln size

nQueensNumSoln size
	|size == 2 = "No solution"
	|size == 3 = "No solution"
	|otherwise = helpQueens size
	
helpQueens size = [(1,2), (4,4)]
	
-- wasn't sure how to declare this but something like this to store the locations of the queens like she proposed
--queenLoc [(Int, Int)]


--conflictRow board = 
--I was thinking something like this to see if a row was conflicted, but didn't actually start it
--	elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

