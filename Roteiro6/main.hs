--Exercício 1

paridade :: [Int] -> [Bool]
paridade l = map (\x -> mod x 2 == 0) l

--Exercício 2

prefixos :: [String] -> [String]
prefixos l = map (take 3) l

--Exercício 3

saudacao :: [String] -> [String]
saudacao l = map ("Oi " ++) l

--Exercício 4

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar p xs = [ x | x <- xs, p x ]

--Exercício 5

pares :: [Int] -> [Int]
pares lst = filtrar (\x -> mod x 2 == 0) lst

--Exercício 6

solucoes :: [Int] -> [Int]
solucoes l = filtrar (\x -> (5*x + 6) < (x*x)) l

--Exercício 7

maior :: [Int] -> Int
maior l = foldr1 (verify) l
     where 
       verify x y = if x >= y then x
                              else y

--Exercício 8

menor_min10 :: [Int] -> Int
menor_min10 l = foldr (verify) 10 l
     where
       verify x y = if x < y then x
                             else y

--Exercício 9

junta_silabasplural :: [String] -> String
junta_silabasplural l = foldr (++) "s" l

--Exercício 10

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

--Bubble sort

troca :: (Ord a) => [a] -> [a]
troca [x] = [x]
troca (x:y:z)
     | x > y = y : troca (x:z)
     | otherwise = x : troca (y:z)

bolhaOrd :: (Ord a) => [a] -> Int -> [a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n - 1)

bolha :: (Ord a) => [a] -> [a]
bolha [] = []
bolha lista = bolhaOrd lista (length lista)

--Selection sort

remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
      | a == x = xs
      | otherwise = x : (remove a xs)

minimo :: (Ord a) => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
      | x <= (minimo xs) = x
      | otherwise = minimo xs

selecao :: (Ord a) => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
        where
          x = minimo xs

--Insertion sort

insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
         | x <= y = (x:y:ys)
         | otherwise = y : (insereOrd x ys)

insercao :: (Ord a) => [a] -> [a]
insercao [] = []
insercao (x:xs) = insereOrd x (insercao xs)

--Quick sort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [ x | x <- xs, x < s ]
                   ++ [s] ++
                   quicksort [ x | x <- xs, x >= s ]

--Exercício 11 (Número de Comparações)

--Auxiliar

junta :: a -> ([a],b) -> ([a],b)
junta x (y,c) = (x:y,c)

--Bubble sort

trocaC :: (Ord a) => ([a],Int) -> ([a],Int)
trocaC ([x],c) = ([x],c)
trocaC (x:y:z,c)
     | x > y = junta y (trocaC (x:z,c+1))
     | otherwise = junta x (trocaC (y:z,c+1))

bolhaOrdC :: (Ord a) => ([a],Int) -> Int -> ([a],Int)
bolhaOrdC (lista,x) 0 = (lista,x)
bolhaOrdC (lista,x) n = bolhaOrdC (trocaC (lista,x)) (n - 1)

bolhaC :: (Ord a) => [a] -> ([a],Int)
bolhaC [] = ([],0)
bolhaC lista = bolhaOrdC (lista,0) (length lista)

--Selection sort

minimoC :: (Ord a) => ([a],Int) -> (a,Int)
minimoC ([],_) = undefined
minimoC ([x],c) = (x,c)
minimoC (x:xs,c)
      | x <= z = (x,c+1)
      | otherwise = minimoC (xs,c+1)
          where
          (z,_) = minimo (xs,c)

removeC :: (Ord a) => a -> ([a],Int) -> ([a],Int)
removeC a ([],c) = ([],c)
removeC a (x:xs,c)
      | a == x = (xs,c)
      | otherwise = junta x (removeC a (xs,c))

selectC :: (Ord a) => ([a],Int) -> ([a],Int)
selectC ([],c) = ([],c)
selectC (xs,c) = junta x (selectC (removeC x (xs,c+y))
       where
       (x,y) = minimoC (xs,0)

selecaoC :: (Ord a) => [a] -> ([a],Int)
selecaoC [] = ([],0)
selecaoC xs = selectC (xs,0)

--Insertion sort

--Quick sort


--Exercício 12
