enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b = if a > b 
                 then []
                 else a : enumFromTo (1 + a) b

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c | a > c && b >= a || a < c && b < a = []
                      | otherwise = a : enumFromThenTo b (2 * b - a) c
                    
somaListas :: [a] -> [a] -> [a]
somaListas [] l = l
somaListas (h:t) l = h : somaListas t l

posicaoNum :: [a] -> Int -> a 
posicaoNum (h:t) a | a == 0 = h
                   | otherwise = posicaoNum t (a-1)

contrario :: [a] -> [a]
contrario [] = []
contrario (x:xs) = contrario xs ++ [x]

pegar :: Int -> [a] -> [a]
pegar _ [] = []
pegar n (x:xs) | n <= 0 = []
               | otherwise = x : pegar (n-1) xs 

dropar :: Int -> [a] -> [a]
dropar n [] = []
dropar n (x:xs) | n <= 0 = x:xs
                | otherwise = dropar (n-1) xs  
            
zip' :: [a] -> [b] -> [(a,b)]
zip' l [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n x | n < 0 = []
             | otherwise = x : replicar (n-1) x 

intersperse' :: a -> [a] ->[a]
intersperse' n [] = [n]
intersperse' _ [l] = [l]
intersperse' n (x:xs) = x : n : intersperse' n xs 

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [a] = [[a]]
group' (h:t) | elem h (head r) = (h : (head r)) : tail r 
             | otherwise = [h] : r 
              where r = group' t 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

heads' :: [[a]] -> [a]
heads' [] = []
heads' ((x:xs):t) = x : heads' t 

total :: [[a]] -> Int
total [[]] = 1
total [[a]] = 2
total ((l):t) = length l + total t 

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = (x,z) : fun t

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t 

idade :: Int -> Int -> [(String,Int)] -> [String]
idade a b [] = []
idade 0 b l = []
idade a 0 l = []
idade a b ((x,y):t) | a - y >= b = x : idade a b t
                    | otherwise = idade a b t 
                
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]

isPrime :: Int -> Bool
isPrime n | n >= 2 = primeCheck n 2
          | otherwise = False 

primeCheck :: Int -> Int -> Bool
primeCheck n m | m * m > n = True
               | mod n m == 0 = False
               | otherwise = primeCheck n (m+1)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf l [] = False
isPrefixOf [] l = True 
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l [] =  False 
isSuffixOf [] l = True
isSuffixOf (x:xs) (y:ys) = x == head ys && isSuffixOf xs ys 

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] l = True  
isSubsequenceOf l [] = False 
isSubsequenceOf (x:xs) (y:ys) = x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n (x:xs) | n == x = 0 : map (+1) (elemIndices n xs)
                     | otherwise = map (+1) (elemIndices n xs)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) | elem h t = nub t 
          | otherwise = h : nub t

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' a | elem l i = nub i
      | otherwise = nub i ++ [l]
    where i = init a
          l = last a

delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs) | n == x = xs
                | otherwise = x : delete n xs 

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) [] l2 = []
(\\) (x:xs) (y:ys) | x == y = (\\) xs ys 
                   | otherwise = x: (\\) xs (y:ys)

union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (y:ys) | elem y l = union l ys
                    | otherwise = union (l ++ [y]) ys

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' l [] = l
intersect' [] l = []
intersect' (x:xs) (y:ys) | x == y = x : intersect' xs (y:ys)
                         | otherwise = intersect' xs ys  
              
insert' :: Ord a => a -> [a] -> [a]   
insert' n [] = [n]
insert' n (x:xs) | n < x = n : (x:xs)
                 |otherwise = x : insert' n xs 

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ (if null xs then "" else " ") ++ unwords' xs

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ (unlines' xs) 

pMaior :: Ord a => [a] -> Int
pMaior (x:xs) | x > (head xs) = 0 
              | otherwise = 1 + r
                where r = pMaior xs

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' n [] = Nothing
lookup' n ((a,b):t) | n == a = Just b 
                   | otherwise = lookup' n t

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:xs) | y >= x = x : preCrescente (y:xs) 
                      | otherwise = [x]

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

menor :: String -> String -> Bool
menor n "" = False
menor "" n = True
menor (x:xs) (y:ys) | x < y = True 
                    | x == y = menor xs ys 
                    | otherwise = False 

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False 
elemMSet n ((a,b):t) | n == a = True 
                     | otherwise = elemMSet n t 

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t 
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)

insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' n [] = []
insereMSet' n ((a,b):t) | a == n = (a,b+1) : insereMSet' n t 
                        | otherwise = (a,b) : insereMSet' n t 


removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((a,1):t) = []
removeMSet n ((a,b):t) | n == a = (a,b-1) : removeMSet n t
                       | otherwise = (a,b) : removeMSet n t

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) | x == (head xs) = (x,r+1) : constroiMSet xs
                    | otherwise = (x,1) : constroiMSet xs
                     where r = 0

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a:as,bs)
                               where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b:bs)
                                where (as,bs) = partitionEithers t 

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a) :t) = a : catMaybes t

data Movimento = Norte 
              | Sul 
              | Este 
              | Oeste
                deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x',y') | x < x' = Este : caminho (x+1,y) (x',y')
                      | x > x' = Oeste : caminho (x-1,y) (x',y')
                      | y < y' = Norte : caminho (x,y+1) (x',y')
                      | y > y' = Sul : caminho (x,y-1) (x',y') 
                      | otherwise = []

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

equadrado :: Rectangulo -> Bool
equadrado (Rect (x1,y1) (x2,y2)) = abs (y2-y1) == abs (x2-x1)

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0 
contaQuadrados (h:t) | equadrado h = 1 + contaQuadrados t
                     | otherwise = contaQuadrados t 

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

data Equipamento = Bom 
                | Razoavel 
                | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t 
naoReparar (Avariado:t) = naoReparar t


unwords'' :: [String] -> String
unwords'' [] = "" 
unwords'' (x:xs) = x ++ (if null xs then "" else " ") ++ unwords'' xs 

fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((a,b,c):t) = (a,c) : fun' t 

heads'' :: [[a]] -> [a]
heads'' [] = []
heads'' ((x:xs):t) = x : heads'' t 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (m:ms) = posicao (case m of Norte -> (x,y+1) 
                                          Sul -> (x,y-1) 
                                          Este -> (x+1,y)
                                          Oeste -> (x-1,y)) ms

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops (a,b) [] = False 
hasLoops posi ms = posi == posicao posi ms || hasLoops posi (init ms)