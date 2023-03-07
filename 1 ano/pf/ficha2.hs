import Data.Char

funA ::[Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0
             then h : (funB t)
             else (funB t)

funC (x:y:t) = funC t 
funC [x] = [x]
funC [] = []

dobros :: [Float] -> [Float] 
dobros [] = []
dobros (x:xs) = x * 2 : dobros xs

somatorio :: [Int] -> Int
somatorio (x:xs) = x + somatorio xs 

numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (x:xs) = if x == c 
                     then 1 + numOcorre c xs
                     else numOcorre c xs
            
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) = if (x > 0) 
                   then (positivos xs) 
                   else False
                    
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if (x >= 0)
               then x : soPos xs
               else soPos xs

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if (x < 0)
                 then x + (somaNeg xs)
                 else somaNeg xs

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if (length (x:xs) <= 3)
                 then (x:xs)
                 else tresUlt xs
            
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = [b] ++ segundos t 

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((c,d):t) = a == c || nosPrimeiros a t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+xa, b+xb, c+xc)
                       where (xa, xb,xc) = sumTriplos t 

soDigitos :: [Char] -> [Char]
soDigitos (x:xs) | isDigit x = x : soDigitos xs
                 | otherwise = soDigitos xs

type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((c,e):t) | n==e = 1 + conta n t 
                  | otherwise = conta n t 

grau :: Polinomio -> Int
grau [(c,e)] = e
grau ((c,e):t) =  max e (grau t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((c,e):t) | n == e = (c,e) :  selgrau n t
                    | otherwise = selgrau n t

deriv :: Polinomio -> Polinomio
deriv [(c,e)] = [(c * (fromIntegral e), e - 1)]
deriv ((c,e):t) = (c * (fromIntegral e), e - 1) : deriv t 

calcula :: Float -> Polinomio -> Float
calcula n [(c,e)] = c * (n ^ (fromIntegral e))
calcula n ((c,e):t) = c * (n ^ (fromIntegral e)) + calcula n t

addMonomio :: Monomio -> Polinomio -> Polinomio
addMonomio m [] = [m]
addMonomio (c,e) ((c',e'):t) | e == e' = (c + c',e) : t 
                             | otherwise = (c',e') : addMonomio (c,e) t

-- acumuladores ficha 4
normaliza :: Polinomio -> Polinomio
--normaliza p = normaliza' [] p

--normaliza' acc [] = acc
--normaliza' acc ((c,e):t) = 
                         let acc' = addMonomio (c,e) acc 
                         in normaliza' acc' t 

normaliza [] = []
normaliza p = let p = normaliza  