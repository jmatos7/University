any' :: (a -> Bool) -> [a] -> Bool 
any' f [] = False
any' f (x:xs) = f x || any' f xs

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = (x:xs)

span' :: (a->Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) | f x = (x:a,b)
               | otherwise = ([],(x:xs))
                 where (a,b) = span' f xs 


deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f _ [] = []
deleteBy' f n (x:xs)| f n x = xs 
                    | otherwise = x : deleteBy' f n xs

type Mat a = [[a]]

transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' ((x:xs):xss) = (x: [h | (h:_) <- xss]) : transpose' (xs:[t| (_:t) <- xss])

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = zipWith (\l1 l2 -> zipWith (*) l1 l2) m1 m2

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

altura :: BTree a -> Int
altura Empty = 0
altura (Node a e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d 

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = (folhas e) + (folhas d)

prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune 0 _ = Empty
prune x (Node r e d) = Node r (prune (x-1) e ) (prune (x-1) e )

path :: [Bool] -> BTree a -> [a]
path [] (Node r e d ) = [r]
path _ Empty = []
path (x:xs) (Node r e d) | x == True = r : path xs d 
                         | otherwise = r : path xs e 

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d ) = Node r (mirror d) (mirror e)

zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r e d ) (Node r2 e2 d2 ) = Node (f r r2) (zipWithBT f e e2) (zipWithBT f d d2)
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (r1,r2,r3) e d) = ((Node r1 (e1) (d1)),     
                                (Node r2 (e2) (d2)),
                                (Node r3 (e3) (d3))) 
                                 where (e1,e2,e3) = unzipBT e
                                       (d1,d2,d3) = unzipBT d

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _ ) = r 
minimo (Node r e d ) = minimo e 

semMinimmo :: Ord a => BTree a -> BTree a 
semMinimmo (Node r Empty Empty) = Empty
semMinimmo (Node r Empty d ) = d
semMinimmo (Node r e d ) = Node r (semMinimmo e) d 

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node r Empty Empty) = (r,Empty)
minSmin (Node r Empty d ) = (r, d) 
minSmin (Node r e d) = (min,e')
                        where (min,e') = minSmin e 