(//) :: Eq a => [a] -> [a] -> [a]
(//) [] _ = []
(//) l [] = l
(//) (x:xs) rem | elem x rem = (//) xs (aux x rem) 
                | otherwise = x : (//) xs rem 

aux :: [a] -> [a]
aux x [] = []
aux x (y:ys) | x == ys = ys 
             | otherwise = y : aux x ys 

type Mset a = [(a,Int)]

removeMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMset x [] = []
removeMset x ((a,num):t) = if x == a
                            then if n == 1 
                                 then t 
                                 else (a,(num-1))
                            else (a,num) : removeMset x t
                        
calcula :: MSet a -> ([a],Int)  
calcula lista = foldl aux ([],0) lista

aux (l,n) (el,n2) = (l++[el],n+n2)

partes :: String -> Char -> [String]
partes "" _= []
partes l y      | elem (head l) y = partes (tail l) y 
                | otherwise = (head l) : partes (tail l) y

data BTree a = Empty | Node a (BTree a) (BTree a)

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node r e d ) | x < r = Node r e remove x r 
                       | x > r = Node r d remove x r
                       | otherwise = aux x (Node r e d)

aux x (Node r Empty d) = d
aux x (Node r e Empty) = e 
aux x (Node r e d) = Node g e h
                     where (g,h) = minSmin d
                    
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty _) = (e,Empty)
minSmin (Node r e d) = (a,Node r b y)
                   where (a,b) = minSmin e


showBTree :: Show a => BTree a -> String
showBTree Empty = "*"
showBTree (Node r e d) = "(" ++ showBTree e ++ "<-" ++ (show r) ++ "->" ++ showBTree d ++ ")"


randomSel' :: Int -> [a] -> IO[a]
randomSel' 0 _ = return [] 
randomSel' _ [] = return []
randomSel' n xs = do r <- randomRIO (0, length xs - 1)
                     let (ys, z:zs) = splitAt r xs 
                     rs <- randomSel' (n-1) (ys ++ zs)
                     return (z:rs)