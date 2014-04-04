{--

Plays and solves the game 2048

Tested with Hugs 98

--}

emptyGrid :: [Int]
emptyGrid = [ 0 | _ <- [0..15] ]

-- Display the 16-length list as the 4 by 4 grid it represents
gridToString :: [Int] -> String
gridToString [] 		= ""
gridToString xs 		= show (take 4 xs) ++ "\n" ++ gridToString (drop 4 xs)

printGrid :: [Int] -> IO()
printGrid xs 			= putStrLn $ gridToString xs

-- Skip n empty tiles before inserting
addTile :: Int -> [Int] -> [Int] 
addTile 0 (0:grid)		= 2 : grid
addTile n [] 			= []
addTile n (0:grid)		= (0 : addTile (n-1) grid)
addTile n (cell:grid)	= cell : addTile n grid

-- Insert multiple tiles at once
addTiles :: [Int] -> [Int] -> [Int]
addTiles [] grid 		= grid
addTiles (n:ns) grid 	= addTiles ns (addTile n grid)

-- For one row of the grid, push the non-empty tiles together
-- e.g. [0,2,0,2] becomes [2,2,0,0]
moveRow :: [Int] -> [Int]
moveRow [] 					= []
moveRow (0:xs) 				= moveRow xs ++ [0]
moveRow (x:xs)				= x : moveRow xs

-- For one row of the grid, do the merge (cells of same value merge)
-- e.g. [2,2,4,4] becomes [4,8,0,0]
-- [2,4,2,2] becomes [2,4,4,0]

mergeRow :: [Int] -> [Int]
mergeRow [] 				= []
mergeRow (a:[]) 			= [a]
mergeRow (a:b:xs) 			
	| a == b 				= (a + b) : (mergeRow xs) ++ [0]
	| otherwise				= a : mergeRow (b:xs)

-- Rotate the grid to be able to do vertical moving/merging
-- e.g. [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
-- becomes [0,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15]
rotate :: [Int] -> [Int]
rotate grid = [ grid !! (a + 4 * b) | a <- [0..3], b <- [0..3] ]

-- Use the definitions above to do the moves
move :: Int -> [Int] -> [Int]
move _ [] 				= []
-- 0=Left, 1=Right, 2=Up, 3=Down
move 0 grid				= mergeRow (moveRow (take 4 grid)) ++ move 0 (drop 4 grid) 
move 1 grid 			= reverse (move 0 (reverse grid))
move 2 grid 			= rotate (move 0 (rotate grid))
move 3 grid 			= reverse (move 2 (reverse grid))

-- Mapping of move-codes to text
moveToString :: Int -> String
moveToString n = ["Left", "Right", "Up", "Down"] !! n

-- Take a turn, i.e. make a move and add a tile
takeTurn :: Int -> [Int] -> [Int]
takeTurn n grid
	| n == -1 				= []
	| newGrid /= grid 		= newGrid
	| otherwise 			= []
	where newGrid = addTile 0 (move n grid)

maxInList :: Ord a => [a] -> a
maxInList (x:xs) = maxInList_ x xs 
maxInList_ :: Ord a => a -> [a] -> a
maxInList_ m [] = m
maxInList_ m (x:xs) 	= maxInList_ (max m x) xs

-- Find highest tuple in list of pairs.
-- On equality, the first wins
maxTuple :: [(Int,Int)] -> Int
maxTuple [] 				= -1
maxTuple (x:xs)				= secondFromTuple $ maxTuple_ x xs
secondFromTuple :: (a,a) -> a
secondFromTuple (x,y) 		= y
maxTuple_ :: Ord a => (a,a) -> [(a,a)] -> (a,a)
maxTuple_ x [] 				= x
maxTuple_ (a,b) ((y,z):xs)
	| a >= y 				= maxTuple_ (a,b) xs
	| otherwise 			= maxTuple_ (y,z) xs

-- Return the best possible move
-- TODO: can the seemingly redundancy be eliminated?
bestMove :: Int -> [Int] -> Int
bestMove depth grid 		= maxTuple [ (gridValue depth (takeTurn x grid), x) | x <- [0..3], takeTurn x grid /= [] ]

gridValue :: Int -> [Int] -> Int
gridValue _ [] = -1
gridValue 0 grid = length $ filter (==0) grid
gridValue depth grid = maxInList [ gridValue (depth-1) (takeTurn x grid) | x <- [0..3] ]

-- Take turns and prints the result of each move to the console until no more moves are possible
-- n counts the moves, nplus is used to track which moves have been tried
-- Should normally be called with n=0 and nplus=0
takeTurns :: Int -> Int -> [Int] -> IO()
takeTurns depth n grid 			= do
	let newGrid 				= takeTurn (bestMove depth grid) grid
	if newGrid /= []
		then do
			putStrLn $ "Move " ++ (show n)
			putStrLn $ gridToString newGrid
			takeTurns depth (n+1) newGrid
		else 
			putStrLn "Game Over"

solve :: Int -> IO()
solve depth = takeTurns depth 0 emptyGrid

main = do
	solve 4
