import Data.List
import Data.Maybe

intersect' :: Eq a => [a] -> [a] -> [a] 
intersect' [] _ = []
intersect' (x:xs) ys | elem x ys = x : intersect' xs ys 
                    | otherwise = intersect' xs ys 
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = (x:xs) : tails' xs  

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems' :: ConjInt -> [Int]
elems' [] = []
elems' ((a,b):t) = [a..b] ++ elems' t 

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = let a = groupBy (\ x y -> x == y-1) l
                 b = map convInt a
             in b

convInt :: [Int] -> Intervalo
convInt l = (head l, last l)

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

agenda1 = [("joana",[Tlm 213041,Email "ajiedmdid"])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email agenda = agenda ++ [(nome,[Email email])]

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(nom,contacto)] |nome == nom = Just (map (\ x -> case x of Email e -> e) contacto)
                                |otherwise = Nothing
verEmails nome ((nom,contacto):agenda) | nome == nom = verEmails nome [(nom,contacto)]
                         |otherwise = verEmails nome agenda 

consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (x:xs) = case x of 
          Casa x -> (x:a,b)
          Tlm x -> (x:a,b)
          Trab x -> (x:a,b)
          Email x -> (a, x :b)
     where (a,b) = consulta xs 

data RTree a = R a [RTree a] deriving (Show, Eq)

paths :: RTree a -> [[a]]
paths (R n []) = [[n]]
paths (R n ns) = map ((:) n. concat. paths) ns

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[h]] = R h []
unpaths [(h:t)] = R h [(unpaths [t])]
unpaths ((h:t):l) = R h ((unpaths [t]):(map unpaths [l]))

type MSet a = [(a,Int)]

cardMSet' :: MSet a -> Int
cardMSet' [] = 0
cardMset' ((a,b):t) = b + cardMSet' t

moda :: MSet a -> [a]
moda [] = []
moda [(a,b)] = [a]
moda ((a,b):(x,y):t) | b >= y = a : moda ((x,y):t)
                     | otherwise = moda ((x,y):t)

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = replicate b a ++ converteMSet t 

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] x y = [(x,y)]
addNcopies ((x,y):t) a b | x == a = insere (x,y+b) t
                         |otherwise = insere (a,b) (addNcopies t a b)

insere :: Eq a => (a,Int) -> MSet a -> MSet a
insere (x,y) [] = [(x,y)]
insere (x,y) ((a,b):t) | y > b = (x,y) : (a,b) : t
                       |otherwise = (a,b) : insere (x,y) t 

transposeMat :: [[a]] -> [[a]]
transposeMat [] = []
transposeMat ([] : xss) = transposeMat xss
transposeMat ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transposeMat (xs : [t | (_:t) <- xss])
