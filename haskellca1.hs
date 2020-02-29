main :: IO ()

isValidISBN13 :: [Integer] -> Bool
isValidISBN13 x
  | length x /= 13 = False
isValidISBN13 x
  | last x == ((10 - (foldr (+) 0 (map (\(x,y) -> if y `mod` 2 == 1 then (x * 3) else (x)) (zip (take 12 x) [0..]))) `mod` 10) `mod` 10) = True
  | otherwise = False

numeralPairs = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
                (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
                (4,"IV"), (1,"I")]


romanToInt :: [Char] -> Integer
romanToInt []
  = 0
romanToInt (x:xs)
  | null xs = getValue([x])
romanToInt (x:y:xs)
  | getValue([x]) < getValue([y]) = getValue([x]++[y]) + romanToInt(xs)
  | otherwise = getValue([x]) + romanToInt(y:xs)

getValue :: [Char] -> Integer
getValue x
  = head $ map fst (filter ((== x) . snd) numeralPairs )

intToRoman :: Integer -> [Char]
intToRoman 0
  = ""
intToRoman x
  | x < 1 = "Enter value greater than 1"
  | x > 4000 = "Enter value less than 4,000"
  | otherwise = b ++ intToRoman (x - a)
      where (a, b) = head ( filter ((<= x) . fst) numeralPairs )

data Tree = EmptyTree | Node Tree Integer Tree deriving (Show)

insertTree :: Tree -> Integer -> Tree
insertTree EmptyTree x
  = Node EmptyTree x EmptyTree
insertTree (Node left c right) x
  | x == c = Node left c right
  | x < c  = Node (insertTree left x) c right
  | x > c  = Node left c (insertTree right x)

buildTree :: [Integer] -> Tree
buildTree []
  = EmptyTree
buildTree x
  = foldl insertTree EmptyTree x

flattenTree :: Tree -> [Integer]
flattenTree EmptyTree
  = []
flattenTree (Node left c right)
  = (flattenTree left) ++ [c] ++ (flattenTree right)

sortList :: [Integer] -> [Integer]
sortList []
  = []
sortList x
  = flattenTree $ buildTree x



wordSearch :: ([[Char]], [String]) -> [String]
wordSearch (grid, [])
  = []
wordSearch (grid, word:words)
  | length(word) == 1 = ["Enter a word longer than length one."]
  | horizontal(grid, word) /= ["Word not found."] = horizontal(grid, word) ++ wordSearch(grid, words)
  | vertical(grid, word)   /= ["Word not found."] = vertical(grid, word) ++ wordSearch(grid, words)
  | diagonal(grid, word)   /= ["Word not found."] = diagonal(grid, word) ++ wordSearch(grid, words)
  | otherwise = [word] ++ ["Word not found."] ++ wordSearch(grid, words)

horizontal :: ([[Char]], String) -> [String]
horizontal ([], word)
  = ["Word not found."]
horizontal (row:rows, word)
  | (search(row, word) == True) = [word] ++ [" RIGHT"]
  | (search(row, reverse word) == True) = [word] ++ [" LEFT"]
  | otherwise = horizontal(rows, word)

vertical :: ([[Char]], String) -> [String]
vertical ([], word)
  = ["Word not found."]
vertical (row:rows, word)
  | length(row) == 1 = if (search((concat (row:rows)), word) == True) then [word] ++ [" DOWN"]
                       else if (search((concat (row:rows)), reverse(word)) == True) then [word] ++ [" UP"]
                       else ["Word not found."]
  | length(row) >  1 = if (search(map (\x -> head(x)) (row:rows), word) == True) then [word] ++ [" DOWN"]
                       else if (search(map (\x -> head(x)) (row:rows), reverse(word)) == True) then [word] ++ [" UP"]
                       else vertical(map (\x -> tail(x)) (row:rows), word)

diagonal :: ([[Char]], String) -> [String]
diagonal ([], word)
  = ["Word not found."]
diagonal (grid, word)
  = if length((filter (== True) [search(x, word) | x <- (diagonalise(grid))])) > 0 &&
      (head (filter (== True) [search(x, word) | x <- (diagonalise(grid))]) == True) then [word] ++ [" UP RIGHT"]
    else if length((filter (== True) [search((reverse(x)), word) | x <- (diagonalise(grid))])) > 0 &&
      (head (filter (== True) [search((reverse(x)), word) | x <- (diagonalise(grid))]) == True) then [word] ++ [" DOWN LEFT"]
    else if length((filter (== True) [search(x, word) | x <- (diagonalise(map reverse $ grid))])) > 0 &&
      (head (filter (== True) [search(x, word) | x <- (diagonalise(map reverse $ grid))]) == True) then [word] ++ [" UP LEFT"]
    else if length((filter (== True) [search((reverse(x)), word) | x <- (diagonalise(map reverse $ grid))])) > 0 &&
      (head (filter (== True) [search((reverse(x)), word) | x <- (diagonalise(map reverse $ grid))]) == True) then [word] ++ [" DOWN RIGHT"]
    else if ((filter (== True) [search(x, word) | x <- (diagonalise(grid))]) == []) then ["Word not found."]
    else ["Word not found."]

diagonalise :: [[Char]] -> [[Char]]
diagonalise []
  = []
diagonalise ([]:xss)
  = xss
diagonalise xss
  = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                 ([]:(diagonalise (map tail xss)))

search :: ([Char], String) -> Bool
search ([], [])
  = True
search ([], l)
  = False
search (c, [])
  = True
search (c:cs, l)
  = if (c == head (l)) && ((take (length(l)) (c:cs)) == l) then True
    else search(cs, l)



main = do
       print(isValidISBN13 [ 9, 7, 8, 0, 1, 3, 7, 0, 5, 3, 4, 6, 9 ])
       print (romanToInt "MCMXCIX")
       print (intToRoman (1999))
       print (buildTree[ 60, 17, 9, 23, 19 ])
       print (flattenTree(Node (Node (Node EmptyTree 9 EmptyTree) 17 (Node (Node EmptyTree 19 EmptyTree) 23 EmptyTree)) 60 EmptyTree))
       print (sortList[1, 4, 5, 3, 9, 11, 44, 2, -10])
       print (wordSearch        (         [['I', 'U', 'P', 'G', 'R', 'A', 'D', 'E', 'E', 'P', 'E', 'Q'],
                                           ['Y', 'T', 'D', 'Z', 'M', 'T', 'Z', 'V', 'N', 'R', 'X', 'S'],
                                           ['Y', 'V', 'C', 'E', 'C', 'T', 'I', 'W', 'A', 'L', 'Z', 'R'],
                                           ['P', 'C', 'P', 'G', 'E', 'R', 'S', 'W', 'G', 'C', 'R', 'E'],
                                           ['P', 'G', 'L', 'U', 'D', 'V', 'D', 'U', 'C', 'F', 'N', 'S'],
                                           ['O', 'N', 'T', 'D', 'J', 'R', 'R', 'W', 'D', 'F', 'O', 'Y'],
                                           ['L', 'V', 'R', 'G', 'A', 'X', 'A', 'I', 'Y', 'F', 'K', 'Z'],
                                           ['F', 'A', 'U', 'H', 'B', 'G', 'S', 'X', 'T', 'E', 'L', 'I'],
                                           ['H', 'E', 'G', 'S', 'P', 'K', 'H', 'W', 'Y', 'P', 'O', 'C'],
                                           ['T', 'E', 'S', 'Z', 'E', 'B', 'A', 'B', 'I', 'D', 'K', 'Y'],
                                           ['N', 'Z', 'W', 'T', 'U', 'R', 'O', 'H', 'O', 'I', 'P', 'K'],
                                           ['M', 'X', 'T', 'G', 'E', 'A', 'D', 'G', 'A', 'V', 'L', 'U'],
                                           ['T', 'E', 'S', 'S', 'R', 'M', 'E', 'M', 'O', 'R', 'Y', 'O'],
                                           ['D', 'I', 'Q', 'D', 'T', 'R', 'O', 'M', 'T', 'K', 'S', 'L'],
                                           ['I', 'R', 'C', 'T', 'L', 'A', 'P', 'T', 'O', 'P', 'O', 'X']],
                                           ["LAPTOP", "KEYBOARD", "BUGS", "DISKETTE", "UPGRADE", "MEMORY",
                                           "HARDWARE", "FLOPPY", "HARDDRIVE", "SOFTWARE"]))
