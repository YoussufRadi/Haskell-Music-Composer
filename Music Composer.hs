import MusicResources
import Data.List

-- %%%%%%%%%%%%%%%%%%%%   Part A   %%%%%%%%%%%%%%%%%%%%%%%%

makeHelper4 :: Char -> [Char] -> [Char]
makeHelper4 c [x] = []
makeHelper4 c [] = []
makeHelper4 c (x:y:xs) | (c==x) = y: makeHelper4 c (y:xs)
		       | otherwise = makeHelper4 c (y:xs)

makeHelper5 :: Char -> [[Char]] -> [Char]
makeHelper5 c [] = []
makeHelper5 c (x:xs) = makeHelper4 c (x) ++ makeHelper5 c (xs)

makeHelper6 :: Char -> [Char]
makeHelper6 c = makeHelper5 c training

possiblePack :: Char -> (Char,[(Int,Char)])
possiblePack c = (c,reverse(sort(possiblePack1 (chars) (makeHelper6 c))))
possiblePack1 :: [Char] -> [Char] -> [(Int,Char)]
possiblePack1 [] cs = []
possiblePack1 (x:xs) cs = pack x (cs) ++ possiblePack1 xs (cs)

pack ::Char -> [Char] -> [(Int,Char)]
pack c xs = if(length (findIndices (==c) xs) /= 0) then [(length (findIndices (==c) xs),c)] else []

makeStatsList :: [(Char,[(Int,Char)])]
makeStatsList  = map possiblePack chars

-- %%%%%%%%%%%%%%%%%%%%   Part B   %%%%%%%%%%%%%%%%%%%%%%%%

sumf :: [(Int,Char)] -> Int
sumf [] = 0
sumf (x:xs) = fst(x) + sumf (xs)

findELm :: Int -> [(Int,Char)] -> (Int,Char)
findELm c (x:xs) | j > 0 = findELm j xs
		 | otherwise = x
		where j = c-fst(x)

getElm1 :: [(Int,Char)] -> (Int,Char)
getElm1 x = (findELm (randomZeroToX (sumf x)) x)
getElm :: [(Int,Char)] -> Char
getElm x = snd(getElm1 x)

findx :: Char -> Char
findx c = findx1 c makeStatsList
findx1 :: Char -> [(Char,[(Int,Char )])] -> Char 
findx1 c [] = error "Invalid Character"
findx1 c (x:xs)  | (contains c) == False = error "Dead End"
		 | c == fst(x) = getElm (snd(x))
	         | otherwise = findx1 c xs

compose :: Char -> Int -> [Char]
compose x k = x :compose1 x (k-1)
compose1 :: Char -> Int -> [Char]
compose1 x 0 = []
compose1 x k = j : (compose1 j (k-1)) where j = findx x

remove :: [(Char,[(Int,Char)])]
remove = remove1 makeStatsList
remove1 :: [(Char,[(Int,Char)])] -> [(Char,[(Int,Char)])]
remove1 [] = []
remove1 (x:xs) | snd(x)==[] = remove1 xs
	       | otherwise = [x] ++ (remove1 xs)

contains :: Char -> Bool
contains c = contains1 (c) (remove)
contains1 :: Eq a => a -> [(a,b)] -> Bool
contains1 c [] = False
contains1 c (x:xs) | (c==fst(x)) = True
		   | otherwise = contains1 c (xs)