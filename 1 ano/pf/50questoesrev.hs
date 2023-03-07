enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x >= y = []
               | otherwise = x : enumFromTo' (x+1) y 

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x > z && y >= x || x > y && y < x = []
                      | otherwise = x : enumFromThenTo' y (2 * y - x ) z

somaListas :: [a] -> [a] -> [a]
somaListas [] l= l
somaListas (x:xs) l = x : somaListas xs l

encontraLista :: [a] -> Int -> a
encontraLista [x] n = x  
encontraLista (h:t) n | n == 0 = h
                      | otherwise = encontraLista t (n-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (h:t) | n <= 0 = []
              | otherwise = h : take' (n-1) t

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (h:t) | n <= 0 = h:t
              | otherwise = drop' (n-1) t 

zip' :: [a] -> [b] -> [(a,b)]
zip' l [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

intersperse' :: a -> [a] -> [a]
intersperse' n [] = [n]
intersperse' n (x:xs) = x : n : intersperse' n xs

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [a] = [[a]]
group' (x:xs) | elem x (head r) = (x : head r) : tail r
              | otherwise = [x] : r
               where r = group' xs 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

inits' :: [a] -> [[a]]
inits' [] = []
inits' l = inits' (init l) ++ [l]

tails' :: [a] -> [[a]]
tails' [] = []
tails' l = [l] ++ tails' (tail l)

heads' :: [[a]] -> [a]
heads' [] = []
heads' [[]] = []
heads' ((x:xs):t) = x : heads' t

total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t 

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t 

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t 

idade :: Int -> Int -> [(String,Int)] -> [String]
idade x y [] = []
idade x y ((a,b):t) | x - b >= y = a : idade x y t
                    | otherwise = idade x y t

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]

primeCheck :: Int -> Int -> Bool
primeCheck n m | m*m > n = True 
               | mod n m == 0 = True
               |otherwise = primeCheck n (m-1)

isPrime :: Int -> Bool
isPrime n | n >= 2 = primeCheck n 2
          |otherwise = False

pMaior :: Ord a => [a] -> Int
pMaior (h:t) | h > (head t) = 0
             | otherwise = 1 + r
               where r = pMaior t

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l = True 
isPrefixOf' (x:xs) (y:ys) | x == y && isPrefixOf' xs ys = True
                         | otherwise = False
                         
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] l = True  
isSuffixOf' (x:xs) (y:ys) | x == (head ys) && isSuffixOf' xs ys = True 
                          | otherwise = False 

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] l = True
isSubsequenceOf (x:xs) (y:ys) | x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys = True
                              | otherwise = False 

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n l = elemIndicesAux n l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux n [] x = []
elemIndicesAux n (h:t) x | n == h = x : elemIndicesAux n t (x+1)
                         | otherwise =  elemIndicesAux n t (x+1)                      

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) | elem x xs = nub xs 
           | otherwise = x : nub xs 

delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (h:t) | n == h = t
               | otherwise = h : delete n t

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] l2 = []
(\\) (x:xs) (y:ys) | x == y = (\\) xs ys 
                   | otherwise = x : (\\) xs (y:ys)

union :: Eq a => [a] -> [a] -> [a]
union [] l = []
union l [] = l
union l (y:ys) | elem y l = union l ys
               | otherwise = union (l ++ [y]) ys

isSuffixOf'' :: Eq a => [a] -> [a] -> Bool
isSuffixOf'' [] l = True
isSuffixOf'' (x:xs) (y:ys) | x == (head ys) && isSuffixOf''  xs ys = True
                           | otherwise = False 

insert' :: Ord a => a -> [a] -> [a]   
insert' n [] = [n]
insert' n (x:xs) | n < x = n : (x:xs)
                 |otherwise = x : insert' n xs 

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert' x (iSort xs)

insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' n [] = []
insereMSet' n ((a,b):t) | a == n = (a,b+1) : insereMSet' n t 
                        | otherwise = (a,b) : insereMSet' n t 

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a:as,bs)
                                where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b:bs) 
                                where (as,bs) = partitionEithers t 

catMaybes :: [Maybe a] -> [a]
catMaybes ((Nothing):t) = catMaybes t 
catMaybes ((Just a):t) = a : catMaybes t 