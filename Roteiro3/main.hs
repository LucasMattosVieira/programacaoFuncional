--Exercício 1
--a)

ou1 :: Bool -> Bool -> Bool
ou1 True True = True
ou1 True False = True
ou1 False True = True
ou1 False False = False

ou2 :: Bool -> Bool -> Bool
ou2 True _ = True
ou2 False a = a

ou3 :: Bool -> Bool -> Bool
ou3 True _ = True
ou3 _ True = True
ou3 _ _ = False

--b)

ou4 :: Bool -> Bool -> Bool
ou4 a b 
     | (a == False) && (b == False) = False
     | otherwise = True

ou5 :: Bool -> Bool -> Bool
ou5 a b
     | a == True = True
     | b == True = True
     | otherwise = False

--Exercício 2

dist :: (Float,Float) -> (Float,Float) -> Float
dist (xa,ya) (xb,yb) = sqrt( (xb-xa)^2 + (yb-ya)^2 )

--Exercício 4

fatorialg :: Int -> Int
fatorialg n
     | n == 0 = 1
     | otherwise = n * fatorialg (n-1) 

fatorialcp :: Int -> Int
fatorialcp 0 = 1
fatorialcp n = n * fatorialcp (n-1)

--Exercício 5

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

--Exercício 6

n_tri :: Int -> Int
n_tri 1 = 1
n_tri n = n + n_tri(n-1)

--Exercício 7

passo :: (Int,Int) -> (Int,Int)
passo (x,y) = (y,x+y)

fibo2 :: Int -> (Int,Int)
fibo2 0 = (0,1)
fibo2 n = passo(fibo2(n-1))


--Exercício 8

potencia2 :: Int -> Int
potencia2 1 = 2
potencia2 n = potencia2(n-1) * 2

--Exercício 9

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
     | m == n = m
     | otherwise = n * prodIntervalo m (n-1)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = prodIntervalo 1 n

--Exercício 11

resto_div :: Int -> Int -> Int
resto_div m n
     | m < n = m
     | otherwise = resto_div (m-n) n

div_inteira :: Int -> Int -> Int
div_inteira m n
     | m < n = 0
     | otherwise = 1 + div_inteira (m-n) n

--Exercício 12

mdcCp :: (Int,Int) -> Int
mdcCp (m,0) = m
mdcCp (m,n) = mdcCp (n,mod m n)

mdcg :: (Int,Int) -> Int
mdcg (m,n)
     | n == 0 = m
     | otherwise = mdcg (n,mod m n)

--Exercício 13

binomialg :: (Int,Int) -> Int
binomialg (n,k)
     | k == 0 = 1
     | k == n = 1
     | otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

binomialcp :: (Int,Int) -> Int
binomialcp (_,0) = 1
binomialcp (n,k) = if n == k then 1
                             else binomialcp (n-1,k) + binomialcp (n-1,k-1)

--Exercício 15

constroi :: Int -> Int -> [Int]
constroi a b
     | a == b = [a]
     | a > b = []
     | otherwise = [a..b]

constroiPar :: Int -> Int -> [Int]
constroiPar a b
     | a == b = []
     | a > b = []
     | mod a 2 == 0 = [(a+2),(a+4)..(b-1)]
     | otherwise = [(a+1),(a+3)..(b-1)]
