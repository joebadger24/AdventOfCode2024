import Data.Array
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.List (nub)

--- Part 1:

program1 :: Array Int Char -> Int -> Int -> [Int]
program1 maze n position =
    case findChar position maze '!' of
        '^' -> move (position - n) '>' '^'
        '>' -> move (position + 1) 'v' '>'
        'v' -> move (position + n) '<' 'v'
        '<' -> move (position - 1) '^' '<'
        _   -> [position]
  where
    move newPos nextDir currentDir
        | findChar newPos maze '!' == '.' = position : program1 (updateMazeDouble maze newPos currentDir position 'X') n newPos
        | findChar newPos maze '!' == 'X' = program1 (updateMazeDouble maze newPos currentDir position 'X') n newPos
        | findChar newPos maze '!' == '#' = program1 (updateMazeSingle maze position nextDir) n position
        | otherwise = [position]

findChar :: Int -> Array Int Char -> Char -> Char
findChar key maze defaultChar = if inRange (bounds maze) key then maze ! key else defaultChar

updateMazeSingle :: Array Int Char -> Int -> Char -> Array Int Char
updateMazeSingle maze position x = maze // [(position, x)]

updateMazeDouble :: Array Int Char -> Int -> Char -> Int -> Char -> Array Int Char
updateMazeDouble maze p1 x1 p2 x2 = maze // [(p1, x1), (p2, x2)]

--- Part 2:

takeStep2 :: Array Int Char -> Int -> Int -> Set.Set (Int, Char) -> Int
takeStep2 maze n position memory
    --- Loop Check:
    | Set.member (position, findChar position maze '!') memory = 1
    --- Up Move:
    -- | findChar position maze '!' == '^' && findChar (position - n) maze '!' == '.' && position < n = 0
    -- | findChar position maze '!' == '^' && findChar (position - n) maze '!' == 'X' && position < n = 0
    -- | findChar position maze '!' == '^' && findChar (position - n) maze '!' == '#' && position < n = 0
    | findChar position maze '!' == '^' && findChar (position - n) maze '!' == '.' =
        takeStep2 (updateMazeDouble maze (position - n) '^' position 'X') n (position - n) (Set.insert (position, '^') memory)
    | findChar position maze '!' == '^' && findChar (position - n) maze '!' == '#' =
        takeStep2 (updateMazeSingle maze position '>') n position (Set.insert (position, '^') memory)
    | findChar position maze '!' == '^' && findChar (position - n) maze '!' == 'X' =
        takeStep2 (updateMazeDouble maze (position - n) '^' position 'X') n (position - n) (Set.insert (position, '^') memory)
    --- Right Move:
    -- | findChar position maze '!' == '>' && findChar (position + 1) maze '!' == '.' && position `mod` n == n - 1 = 0
    -- | findChar position maze '!' == '>' && findChar (position + 1) maze '!' == 'X' && position `mod` n == n - 1 = 0
    | findChar position maze '!' == '>' && findChar (position + 1) maze '!' == '.' =
        takeStep2 (updateMazeDouble maze (position + 1) '>' position 'X') n (position + 1) (Set.insert (position, '>') memory)
    | findChar position maze '!' == '>' && findChar (position + 1) maze '!' == '#' =
        takeStep2 (updateMazeSingle maze position 'v') n position (Set.insert (position, '>') memory)
    | findChar position maze '!' == '>' && findChar (position + 1) maze '!' == 'X' =
        takeStep2 (updateMazeDouble maze (position + 1) '>' position 'X') n (position + 1) (Set.insert (position, '>') memory)
    --- Down Move:
    -- | findChar position maze '!' == 'v' && findChar (position + n) maze '!' == '.' && position >= n * (n - 1) = 0
    -- | findChar position maze '!' == 'v' && findChar (position + n) maze '!' == 'X' && position >= n * (n - 1) = 0
    | findChar position maze '!' == 'v' && findChar (position + n) maze '!' == '.' =
        takeStep2 (updateMazeDouble maze (position + n) 'v' position 'X') n (position + n) (Set.insert (position, 'v') memory)
    | findChar position maze '!' == 'v' && findChar (position + n) maze '!' == '#' =
        takeStep2 (updateMazeSingle maze position '<') n position (Set.insert (position, 'v') memory)
    | findChar position maze '!' == 'v' && findChar (position + n) maze '!' == 'X' =
        takeStep2 (updateMazeDouble maze (position + n) 'v' position 'X') n (position + n) (Set.insert (position, 'v') memory)
    --- Left Move:
    -- | findChar position maze '!' == '<' && findChar (position - 1) maze '!' == '.' && position `mod` n == 0 = 0
    -- | findChar position maze '!' == '<' && findChar (position - 1) maze '!' == 'X' && position `mod` n == 0 = 0
    | findChar position maze '!' == '<' && findChar (position - 1) maze '!' == '.' =
        takeStep2 (updateMazeDouble maze (position - 1) '<' position 'X') n (position - 1) (Set.insert (position, '<') memory)
    | findChar position maze '!' == '<' && findChar (position - 1) maze '!' == '#' =
        takeStep2 (updateMazeSingle maze position '^') n position (Set.insert (position, '<') memory)
    | findChar position maze '!' == '<' && findChar (position - 1) maze '!' == 'X' =
        takeStep2 (updateMazeDouble maze (position - 1) '<' position 'X') n (position - 1) (Set.insert (position, '<') memory)
    --- Otherwise:
    | otherwise = 0

replaceIndices :: String -> [Int] -> [String]
replaceIndices str indices = [replaceAtIndex str idx '#' | idx <- indices]

replaceAtIndex :: String -> Int -> Char -> String
replaceAtIndex str idx newChar =
    let (before, after) = splitAt idx str
    in case after of
        []     -> str
        (_:xs) -> before ++ (newChar : xs)

remove_one :: [Int] -> Int -> [Int]
remove_one = \list -> \v -> 
    case list of 
        [] -> error "Element not found!"
        x:xs | v==x -> xs
        x:xs -> x:remove_one xs v

program2 :: [String] -> Int -> Int -> Int 
program2 [] _ _ = 0
program2 (x:xs) n start = takeStep2 (listArray (0, length x - 1) x) n start Set.empty + program2 xs n start

--- Main Program:

main :: IO ()
main =  do
    let n = 130
    let maze = listArray (0, length input - 1) input
    let start = (fst . head . filter (\(_, c) -> c == '^') $ assocs maze)
    print "Result 1:"
    let result1 = program1 maze n start
    print (length result1)
    print "Result 2:"
    let mazeList = replaceIndices input (remove_one (nub result1) start)
    print (program2 mazeList n start)

--- Data:

test :: String = "....#..............#............#..............#.............#..^.............#.#...............#..."

input :: String = "............#...........................................#.......#.............#...........#.................................................#....................#.........................................#............#............#.....##...................#....#.........#..........#.....#......##...............................................................#..................#.....................#.#...................#..................#.......................#........#............#..................................#..........................#............................#..................#..................#..........##..................#...............................................................................#...................................#..........#.#...#......##........#.........#...........#...............................#...................#........#..#............#.........##......................................#...................................................................#..............................................#......................#.............................................#..........................................................................................#...........#........................#...................#......#.#.........##..................#..#..........#...........................#.#..........#.......##....................#.#....#..#............................#...#..........................#.............................................................................#.......................................#.............#.........#...............................................................................#......#........#.#...............#.........................#.......#.......#.........................#.........................#....#........................##.................#..#...............................#..........#.........................................#......#..............................................................#...................#..........#.........#.#......#.............#.........#.................................#....................#....................#......................#..........#.............................................#..................#.........#...#................#........#....#..........#...........#......................................#.......#..........................................................#...................#..#..............#.........#......#...........................................#..............................###.....#..............................................................#........#..............................#..................#.......................................................#.....#.........................................................................#.....................................#...........#....................................#.........#...............................................#........#...................................................................#...................................#................#..#.................................................#........................#...........................................................................................................##..#......................#.......#...........#..#........#..................#.....................................................................................................................................#......#.........#...............##...............................#......#......................#......#...............#......#...............................................#.#........................#....#.....#..#................##.......................#.....#.............#.........#.............................#.................#.........##.......#..........................#..............................................................................#....................#...........................#...#.....#..............#.................................................#..............................................#..........................................................#...#..................................................................#....................#................#.......................................................................#.........#...........................#..............................#.....#.........#..#............................#................................................................#............#..................................#...#.....#......#.................................................#......................#...#....#......#.....................#..................................#.....#........#.....................#...#........#.........................................#..#........................................................#......#...............#......#.........##.....#.......................#..............#....................................#...#.........................#..........................................................................................#.............#....................#.............#..............##...................................................#................................................................#.......#.......................................#...........#......................................#..............#............................#.#..........................................................................................#.......#...........................#...........................#..............................#....................#........#...#...#.................#..........................................................#............#.....................#.#.....#...#......#.............#...............................................................................#...##..........................#.....#.......#............#........................#..........................................#.............#............................................................#.................#.........................#.#.......#......................................................#...............................#.#............#................#...................#...............#..#...........#........................#............................................#...........#............#...........................................#...#...........................#............#..#..#............#..#..............................#...............#...........#...............#.............................................................................#...........................................#................................#....#....#...#..................#...................#.#..........##.....#.#.......................................................................................#.....#..........#..........#............#......#.......................................................#.......#.#............................#........................#.................#...#........#...#...........................#...#.................^......#............#..................................#................##..........................................................................#....................................................................................#....#............................#........#............................#......#....#..#.......#....................#...............................................#........#.................................................##..............##.......................#...#...............#........................#................#....#..................................................................................................#..........#........................................#...................................................................................#............#.....#...............................##.#............................................#........#.#.......#.#....#...#.............................#...............#.................................................................................#..#...........##............................................................................#.#.....#.........#...............#.....................................................................................#.......#.........#............#....#..........#.....................#.....#..............................................#..#..................#....................#.......................#....#....#.....................#...............#..............................................................#....#...........................#..#...............#..#....................#...........................................#............................................................................#...........................................#.........................#..#...............................#......#........................................#....................................................#......................#......................#........................#...............#......................#...........#.....................................#.......#..................#.......................#.........#.....................................................#.....#..................#.....................#...#..........................................................#.............#................#........................#......#...#.....................#....##.........#.........................................................................#....................#....................................................#..........#..................#..........#..........#.....................#.................................................#...........................................................#.......................................................#....##...................................#...........................................#.............................................#.....#..............#................................#.....#.......#..................................#........................#........................................................##........#.#........#.........................................#..............#......................................#.................................................................................................#................................................................#......#...........................................................................#................................#...................#.............................................................#.........#......................................................#.................................................#................#...................................#...............................#......................................#.........................#....#......#................#..............#...........................................................................................................................#......#.#.................#.#.......#.........#...#................#...............#..............#.....................#........#.........#....#................#..........#........#...................#.............#...............#..........#...#........................#...............#.....#.....#...........#....#....................#.....#...................#............................................#.......................#...........................................................#.....................##.................................................................................................#........................................................................##..#........#............................#............#........................#............................#...............#............#...................................................................................#................#............#..............................................................#.#.................###.................#...........................#.......#....#...............#....................................................#.....#.........#.......##.....................#................#.....................................................#.#......................#.................................................................#.................................##.................................#..............................................#................................................................#....#........................#................................................#..............................................#...#...........#....................................##...............................................................................#.........................#.................#..#.........................#..........................#.......#..........................................##........#.#...#......................................#..............#...............................#.............#..................#.............................................##..............................#............................#...........................................................#..........................#.........#...#...............................#.........#............#......................#.....................................#.....##.........................#...............................#......#............#...................#.......................#..##........................................#..........................................##......#.........#.......#..................##...............#..........................................#.........#...##.........#.#......#.........#.......#.....#.......................#.........................................................................#..............#................#.....#..............##...............#.........................................#..........................................#..#.......#....................................#........................................................................#...........#..........................#.................#...............................#....................................................#..........#........................................#.........................................#..............................................#.................................................#.........#....................................#.......................#.#.......#............##........#........................#..........#..................................#.....#...........................................................#...#........#............................#...........#.........................................................#...............#....................#....#..................#...........#..............................#..........#........#............................#..........#..................#.............................................#..........#....#.................#...................#...#......................#....................#.................#.......#..............#............................#..#.......................#...#.#.....#..........................#.....#...................................#...##.....#.....#...........#................#......#..##................#.........#.........#...#.#.................................................................................................................................................#..#...............#..#...............#.....#....#.............#..#...............#.........#.......#....#.........#...#......#........#.............................#.................................#.....................................#.......#..................#..................#............#...#......#.........#..................##............#.............#..............#.....#........#.................#..#..........#......................#..#............#......#.............#....#...............#.......#....................##.................................................##...............#........#........#........................#.#........#.............#...................................#...##.....................#........................#.......#......#..........................#................#............#...#....#.......#.................#.....................#..........#.....................#...................##..........................#........"

--- Helper Methods for Debugging

printGrid :: Int -> Array Int Char -> String
printGrid n grid =
    let chars = elems grid
        rows = chunk n chars
    in unlines rows

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

timeIt :: IO a -> IO a
timeIt action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    putStrLn $ "Elapsed time: " ++ show (diffUTCTime end start)
    return result