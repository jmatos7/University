import Data.Char

perimetro :: Float -> Float
perimetro r = 2 * pi * r

distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

primUlt :: [Int] -> (Int,Int)
primUlt a = (head a, last a)

multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0
               then True
               else False
        
truncaImpar :: [Int] -> [Int]
truncaImpar a = if mod (length a) 2 == 0
                then a
                else tail a

max2 :: Int -> Int -> Int
max2 a b = if a > b 
           then a
           else b 
           
max3 :: Int -> Int -> Int -> Int
max3 a b c = if (max2 a b) > c
             then (max2 a b)
             else c
             
nRaizes :: Float -> Float -> Float -> (Float, Float)
nRaizes a b c = ( ((-b) + sqrt(b^2 - 4 * a * c)) / (2 * a),
                  ((-b) - sqrt(b^2 - 4 * a * c)) / (2 * a) )

raizes :: Float -> Float -> Float -> [(Float, Float)]
raizes a b c = [nRaizes a b c]

type Hora = (Int, Int)
horaValida :: (Int,Int) -> Bool
horaValida (a,b) = if a < 23 && b < 60 
                   then True
                   else False