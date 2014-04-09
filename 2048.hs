{--

Plays and solves the game 2048

In this implementation the randomized aspects of the game have been removed.

--}
import Data.Time
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad

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
addTile _ [] 			= []
addTile n (0:grid)		= (0 : addTile (n-1) grid)
addTile n (cell:grid)	= cell : addTile n grid

-- For one row of the grid, push the non-empty tiles together
-- e.g. [0,2,0,2] becomes [2,2,0,0]
moveRow :: [Int] -> [Int]
moveRow [] 				= []
moveRow (0:xs) 			= moveRow xs ++ [0]
moveRow (x:xs)			= x : moveRow xs

-- For one row of the grid, do the merge (cells of same value merge)
-- e.g. [2,2,4,4] becomes [4,8,0,0]
-- [2,4,2,2] becomes [2,4,4,0]

mergeRow :: [Int] -> [Int]
mergeRow [] 			= []
mergeRow (a:[]) 		= [a]
mergeRow (a:b:xs)
	| a == b 			= (a + b) : (mergeRow xs) ++ [0]
	| otherwise			= a : mergeRow (b:xs)

-- Rotate the grid to be able to do vertical moving/merging
-- e.g. [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
-- becomes [0,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15]
rotate :: [Int] -> [Int]
rotate grid = [ grid !! (a + 4 * b) | a <- [0..3], b <- [0..3] ]

data Move = MoveLeft | MoveRight | MoveUp | MoveDown
	deriving (Show)

allMoves :: [Move]
allMoves = [MoveLeft, MoveRight, MoveUp, MoveDown]

-- Use the definitions above to do the moves
doMove :: Move -> [Int] -> [Int]
doMove _ [] 			= []
-- 0=Left, 1=Right, 2=Up, 3=Down
doMove MoveLeft grid	= mergeRow (moveRow (take 4 grid)) ++ doMove MoveLeft (drop 4 grid) 
doMove MoveRight grid 	= reverse (doMove MoveLeft (reverse grid))
doMove MoveUp grid 		= rotate (doMove MoveLeft (rotate grid))
doMove MoveDown grid 	= reverse (doMove MoveUp (reverse grid))

-- Take a turn, i.e. make a move and add a tile
-- Return Nothing if the move is not legal
takeTurn :: Move -> [Int] -> Maybe [Int]
takeTurn move grid
	| movedGrid == grid	= Nothing
	| otherwise 		= Just $ addTile 0 movedGrid
	where movedGrid = doMove move grid

-- Return the best move
-- Will throw an error if there are no valid moves
bestMove :: Int -> [Int] -> Move
bestMove depth grid 	= snd bestValueMove
	where 
		valueMoves	 	= [ (value, move) |
							move	<- allMoves,
							newGrid <- [ takeTurn move grid ],
							value 	<- [ gridValue depth (fromJust newGrid) ],
							newGrid	/= Nothing ]
		bestValueMove 	= maximumBy (comparing fst) valueMoves

-- <<< I decided not to return Nothing on a dead end, because then we can no longer
-- distinguish dead ends at different depths from eachother. >>>
--
-- Return the value of the grid,
-- + 1 for each depth traversed
-- -100 if a Game Over position is reached
gridValue :: Int -> [Int] -> Int
gridValue depth grid
	| depth == 0		= length $ filter (==0) grid
	| values == [] 		= -100
	| otherwise 		= maximum values
	where
		values 			= [ value | 
							move	<- allMoves,
							newGrid	<- [ takeTurn move grid ],
							value 	<- [ gridValue (depth-1) (fromJust newGrid) + 1],
							newGrid /= Nothing ]

gameOver :: [Int] -> Bool
gameOver grid 		= all (==Nothing) ([ takeTurn move grid | move <- allMoves ])

-- Take turns and prints the result of each move to the console until no more moves are possible
-- n counts the moves
-- Should normally be called with n=0
takeTurns :: Bool -> Int -> Int -> [Int] -> IO()
takeTurns isVerbose depth n grid = do
	when (isVerbose || isGameOver) 		$ putStrLn $ gridToString grid ++ "\n# " ++ (show n)
	when (isVerbose && not isGameOver) 	$ putStrLn $ (show move)
	when (not isGameOver) 				$ takeTurns isVerbose depth (n+1) newGrid
	where
		isGameOver 	= gameOver grid
		move 		= bestMove depth grid
		newGrid 	= fromJust $ takeTurn move grid

-- Solves at depth and only prints # of moves and final grid
solveSilent :: Int -> IO()
solveSilent depth = takeTurns False depth 0 (addTile 0 emptyGrid)

-- Solves at depth and prints all boards and moves
solve :: Int -> IO()
solve depth = takeTurns True depth 0 (addTile 0 emptyGrid)

-- Solve and time the solver for multiple depth settings from start to end
solveDepths :: Int -> Int -> IO()
solveDepths start end
	| start <= end = do
			startTime <- getCurrentTime
			solveSilent start
			stopTime <- getCurrentTime
			putStrLn $ "Depth " ++ (show start) ++ " done in " ++ (show $ diffUTCTime stopTime startTime)
			solveDepths (start+1) end
	| otherwise	= putStrLn "-"

main = solveDepths 1 3 
