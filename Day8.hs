import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

--- Part 1:

program1 :: Int -> Map.Map Char [Int] -> Int
program1 n m = length (nub (countAnti1 n (getNodes m)))

countAnti1 :: Int -> [(Int, Int)] -> [Int]
countAnti1 _ [] = []
countAnti1 n (x:xs)
    | antiIndex n (fst x) (coords n (fst x)) (snd x) (coords n (snd x)) /= -1 = antiIndex n (fst x) (coords n (fst x)) (snd x) (coords n (snd x)) : countAnti1 n xs
    | otherwise = countAnti1 n xs

getNodes :: Map.Map Char [Int] -> [(Int, Int)]
getNodes m = concatMap compareAll $ Map.elems m

buildMap :: [(Int, Char)] -> Map.Map Char [Int]
buildMap = foldl updateMap Map.empty
  where
    updateMap :: Map.Map Char [Int] -> (Int, Char) -> Map.Map Char [Int]
    updateMap m (x, '.') = m
    updateMap m (x, c) = Map.insertWith (++) c [x] m

compareAll :: [Int] -> [(Int, Int)]
compareAll xs = [(x, y) | x <- xs, y <- xs, x /= y]

antiIndex :: Int -> Int -> (Int, Int) -> Int -> (Int, Int) -> Int
antiIndex n x (x1,x2) y (y1,y2)
    | x1 < y1 && x2 < y2 && abs (x `mod` n - y `mod` n) <= n - 1 - y1 && abs (x `div` n - y `div` n) <= n - 1 - y2 
        = y + abs (x `mod` n - y `mod` n) + abs (x `div` n - y `div` n) * n
    | x1 < y1 && x2 > y2 && abs (x `mod` n - y `mod` n) <= n - 1 - y1 && abs (x `div` n - y `div` n) <= y2 
        = y + abs (x `mod` n - y `mod` n) - abs (x `div` n - y `div` n) * n
    | x1 > y1 && x2 < y2 && abs (x `mod` n - y `mod` n) <= y1 && abs (x `div` n - y `div` n) <= n - 1 - y2 
        = y - abs (x `mod` n - y `mod` n) + abs (x `div` n - y `div` n) * n
    | x1 > y1 && x2 > y2 && abs (x `mod` n - y `mod` n) <= y1 && abs (x `div` n - y `div` n) <= y2 
        = y - abs (x `mod` n - y `mod` n) - abs (x `div` n - y `div` n) * n
    | otherwise = -1

coords :: Int -> Int -> (Int, Int)
coords n index = (index `mod` n, index `div` n)

--- Part 2:

program2 :: Int -> Map.Map Char [Int] -> [(Int, Char)] -> Int
program2 n m zipData = length (nub (countAnti2 n (getNodes m) ++ antennaCount zipData))

countAnti2 :: Int -> [(Int, Int)] -> [Int]
countAnti2 _ [] = []
countAnti2 n (x:xs)
    | antiIndex n (fst x) (coords n (fst x)) (snd x) (coords n (snd x)) /= -1 = 
        let result = antiIndex n (fst x) (coords n (fst x)) (snd x) (coords n (snd x)) 
        in result : countAnti2 n ((snd x, result) : xs)
    | otherwise = countAnti2 n xs

antennaCount :: [(Int, Char)] -> [Int]
antennaCount [] = []
antennaCount ((y, x):zs)
    | x /= '.' =  y : antennaCount zs
    | otherwise = antennaCount zs

--- Main Program:

main :: IO ()
main =  do
    let myData = input
    let zipData = zip [0..] myData
    let n = round (sqrt (fromIntegral (length zipData) :: Double))
    print "Result 1:"
    print $ program1 n (buildMap zipData)
    print "Result 2:"
    print $ program2 n (buildMap zipData) zipData

--- Data:

test :: String = "....................0........0.............0........0.............A.....................................A............A.........................."
input :: String = ".......................V.........e...O......................q.pj8...............................u......................8...........................................8.....6.................J....l....u..........................6................J..Z..B........e.........E...........................O.J.........Jq..........................5..............................E...........e.Q..5.f.............................................Q..A.....f..B.....O.....V...................j.....Af..............................8......n..............l...f....Z7....................n..........4........A........BD...............j...................Q..z.......R....l..N.........6....q.....3....n.........D...........Z..............a.6..3.F........D..I..............................03.................Q.......h...2...........................A.u.......................m..V........F......L.............5..........z.R....Z.......N....q.................n.......L.E...........................M.........y...........................N............................m.L..y........R.o....................L...........I...7..R..............o..........9..............2.......D...........od.............y...........................I....d........3.....M...........E.............I..............X.W....................p.2.....7...z....s...V......o........M.....9.................G......7...................M.....................h..0....m.........d.......F......p.........s.h........z........r..........Y.i................9............s.........W..a.Y..........y.............p...................g.......r........w...............................r.....b...............g........x.s.....h..........a.....d............................................................S.......w.............1........Y...............................H..................b...........Y........................e..t...0.v..........i..........w.........9....T........v.....................U...........2.............................S........t......T............................................U..................Gt.............U...S..........................P.....1.B.......r...X............w.......P.....x.j..................W......x..b........g........F.....a............S.i.................................1.......H............U......b......x.....X..........G.1............i....X....................P..4........H.........................................H......................W...................T4...g................v...........................v........GP..4.....t...."
