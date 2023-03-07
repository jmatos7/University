import Data.List.Split (splitOn)

-- Exercício 1
unlines' :: [String] -> String
unlines' [x] = x
unlines' (x:xs) = x ++ "/n" ++ unlines' xs 

-- exercico 2 
-- a)
stringToVector :: String -> [Int]
stringToVector s = map read $ splitOn "," $ s 

 {-
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter str = go str []
    where go s acc
            | s' == []   = reverse (s:acc)
            | otherwise = go (drop (length delimiter) s') (s:acc)
            where s' = take (length delimiter) s
-}

comma :: [String] -> String
comma [x] = x
comma (h:t) = h ++ "," ++ comma t

type Matrix = [[Int]]

stringToMatrix :: String -> Matrix
stringToMatrix s = map stringToVector (lines s)


transposta :: [[a]]-> [[a]]
transposta []= []
transposta ([]:t) = transposta t 
transposta ((x:xs):xss) = (x : [h | (h:_) <- xss ]) : transposta (xs : [t | (_:t) <- xss])

-- 2)
-- a)

data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula deriving (Show)

semUltimo :: Lista a -> Lista a 
semUltimo (Esq x Nula) = Nula
semUltimo (Dir Nula x) = Nula
semUltimo (Esq x l) = Esq x (semUltimo l)
semUltimo (Dir l x) = l

-- b)

toList :: Lista a -> [a]
toList Nula = []
toList (Dir l x) = toList l ++ [x]
toList (Esq x l) = x : toList l

showListaA :: Show a => Lista a -> String
showListaA = show . toList

-- 3)
-- a)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node r e d) = (inorder e) ++ (r : inorder d)

numera :: BTree a -> BTree (Int,a)
numera t = snd $ numeraAux 1 t

numeraAux :: Int -> BTree a -> (Int,BTree (Int,a))
numeraAux n Empty = (n, Empty)
numeraAux n (Node r e d) = (n2, Node (n1,r) e' d')
                         where
                            (n1,e') = numeraAux n e
                            (n2,d') = (n1+1) d
