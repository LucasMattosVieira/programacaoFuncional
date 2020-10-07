{-
    Trabalho 2 de Programação Funcinal
    Feito em dupla com o aluno Lucas Marçal Coutinho - 11911BCC012
-}

{-
    Listas para exemplos
-}

l1 :: [Int]
l1 = [1..1000]

l2 :: [Int]
l2 = [1000,999..1]

l3 :: [Int]
l3 = l1 ++ [0]

l4 :: [Int]
l4 = [0] ++ l2

l5 :: [Int]
l5 = l1 ++ [0] ++ l2

l6 :: [Int]
l6 = l2 ++ [0] ++ l1

l7 :: [Int]
l7 = l2 ++ [0] ++ l2

x1 :: [Int]
x1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

x2 :: [Int]
x2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

x3 :: [Int]
x3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]

x4 :: [Int]
x4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]

x5 :: [Int]
x5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]

x6 :: [Int]
x6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]

x7 :: [Int]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

{-
    Questão 1
-}

{- Letra A -}

remove :: Ord t => t -> [t] -> [t]
remove x [] = []
remove x (y : r)
    | x == y = r
    | otherwise = y : remove x r

selecao1V1 :: Ord t => [t] -> [t]
selecao1V1 [] = []
selecao1V1 l = [x] ++ selecao1V1 (remove x l)
    where 
        x = foldr1 (\x -> \y -> if x < y then x else y) l

{- Letra B -}

insereOrd1V1 :: Ord t => t -> [t] -> [t]
insereOrd1V1 x [] = [x]
insereOrd1V1 x (y : r)
    | x <= y = x : y : r
    | otherwise = y : insereOrd1V1 x r

insercao1V1 :: Ord t => [t] -> [t]
insercao1V1 [] = []
insercao1V1 (x : r) = foldr (insereOrd1V1) [x] (insercao1V1 r)

{- Letra C -}

quicksort1V1 :: Ord t => [t] -> [t]
quicksort1V1 [] = []
quicksort1V1 (s:xs) = quicksort1V1 (filter (<s) xs) ++ [s] ++ quicksort1V1 (filter (>=s) xs)

{-
    Questão 2
-}

{- Variação 1 -}

junta :: a -> ([a],b) -> ([a],b)
junta x (y,c) = (x:y,c)

troca2V1 :: (Ord a) => ([a],Int) -> ([a],Int)
troca2V1 ([x],c) = ([x],c)
troca2V1 (x:y:z,c)
     | x > y = junta y (troca2V1 (x:z,c+1)) 
     | otherwise = junta x (troca2V1 (y:z,c))

bolhaOrd2V1 :: (Ord a) => [a] -> [a]
bolhaOrd2V1 lista = if x == 0 then lista
                           else bolhaOrd2V1 l
           where
              (l,x) = troca2V1 (lista,0)

bolha2V1 :: (Ord a) => [a] -> [a]
bolha2V1 [] = []
bolha2V1 lista = bolhaOrd2V1 lista

{- Variação 2 -}

troca2V2 :: Ord t => ([t], Int) -> ([t], Int)
troca2V2 ([x], k) = ([x], k)
troca2V2 (l, 0) = (l, 0)
troca2V2 (x : y : r, k)
    | x > y = junta y (troca2V2 (x : r, k - 1))
    | otherwise = junta x (troca2V2 (y : r, k))

bolhaOrd2V2 :: Ord t => [t] -> Int -> [t]
bolhaOrd2V2 l 0 = l
bolhaOrd2V2 l n = bolhaOrd2V2 (fst (troca2V2 (l, n - 1))) (n - 1)

bolha2V2 :: Ord t => [t] -> [t]
bolha2V2 [] = []
bolha2V2 l = bolhaOrd2V2 l (length l)

{- Variação 3 -}

removeUlt :: [a] -> ([a],a)
removeUlt [x] = ([],x)
removeUlt (x:y) = junta x (removeUlt y)

troca2V3 :: (Ord a) => ([a],Int) -> ([a],Int)
troca2V3 ([x],c) = ([x],c)
troca2V3 (x:y:z,c)
     | x > y = junta y (troca2V3 (x:z,c+1)) 
     | otherwise = junta x (troca2V3 (y:z,c))

bolhaOrd2V3 :: (Ord a) => [a] -> [a]
bolhaOrd2V3 lista = if x == 0 then lista
                            else (bolhaOrd2V3 ls) ++ [u]
           where
              (l,x) = troca2V3 (lista,0)
              (ls,u) = removeUlt l

bolha2V3 :: (Ord a) => [a] -> [a]
bolha2V3 [] = []
bolha2V3 lista = bolhaOrd2V3 lista

{- Variação 1 com contador -}

troca2V1C :: (Ord a) => ([a],Int,Int) -> ([a],Int,Int)
troca2V1C ([x],c,k) = ([x],c,k)
troca2V1C (x:y:z,c,k)
     | x > y = aux_concat y (troca2V1C (x:z,c+1,k+1)) 
     | otherwise = aux_concat x (troca2V1C (y:z,c,k+1))

bolhaOrd2V1C :: (Ord a) => ([a],Int) -> ([a],Int)
bolhaOrd2V1C (lista,k) = if x == 0 then (lista,nk)
                           else bolhaOrd2V1C (l,nk)
           where
              (l,x,nk) = troca2V1C (lista,0,k)

bolha2V1C :: (Ord a) => [a] -> ([a],Int)
bolha2V1C [] = ([],0)
bolha2V1C lista = bolhaOrd2V1C (lista,0)

{- Variação 2 com contador -}

aux_concat :: t -> ([t], Int, Int) -> ([t], Int, Int)
aux_concat j (l, k, c) = (j : l, k, c)

troca2V2C :: Ord t => ([t], Int, Int) -> ([t], Int, Int)
troca2V2C ([x], k, c) = ([x], k, c)
troca2V2C (l, 0, c) = (l, 0, c)
troca2V2C (x : y : r, k, c)
    | x > y = aux_concat y (troca2V2C (x : r, k - 1, c + 1))
    | otherwise = aux_concat x (troca2V2C (y : r, k, c + 1))

bolhaOrd2V2C :: Ord t => ([t], Int) -> Int -> ([t], Int)
bolhaOrd2V2C (l, c) 0 = (l, c)
bolhaOrd2V2C (l, c) n = bolhaOrd2V2C (lista, contador) (n - 1)
    where 
        (lista, _, contador) = (troca2V2C (l, n - 1, c))

bolha2V2C :: Ord t => [t] -> ([t], Int)
bolha2V2C [] = ([], 0)
bolha2V2C l = bolhaOrd2V2C (l, 0) (length l)

{- Variação 3 com contador -}

junta2V3C :: a -> ([a],b,Int) -> ([a],b,Int)
junta2V3C x (y,c,k) = (x:y,c,k)

concat2V3C :: a -> ([a],Int) -> ([a],Int)
concat2V3C x (y,k) = (y ++ [x],k)

troca2V3C :: (Ord a) => ([a],Int,Int) -> ([a],Int,Int)
troca2V3C ([x],c,k) = ([x],c,k)
troca2V3C (x:y:z,c,k)
     | x > y = junta2V3C y (troca2V3C (x:z,c+1,k+1)) 
     | otherwise = junta2V3C x (troca2V3C (y:z,c,k+1))

bolhaOrd2V3C :: (Ord a) => ([a],Int) -> ([a],Int)
bolhaOrd2V3C (lista,k) = if x == 0 then (lista,k2)
                                 else concat2V3C u (bolhaOrd2V3C (ls,k2))
           where
              (l,x,k2) = troca2V3C (lista,0,k)
              (ls,u) = removeUlt l

bolha2V3C :: (Ord a) => [a] -> ([a],Int)
bolha2V3C [] = ([],0)
bolha2V3C lista = bolhaOrd2V3C (lista,0)

{- 
    Comparações 
    A primeira variação tem vantagem sobre listas que já estão mais ordenadadas e desvantagem sobre listas completamente desordenadas.
    A segunda variação tem vantagem sobre listas completamente deordenadas e, em seu pior caso, invertidas. Porém, como esse algoritmo não para quando a lista está ordenada antecipadamente, ele faz cáculos desnecessários nessa situação.
    A terceira variação tem as vantagens da primeira e segunda variações, anulando os pontos fracos uma da outra.
    O algoritmo original faz o número máximo de comparações, seja para listas parcialmente ordenadas ou não.
-}

{- 
    Melhor algoritmo
    Como a terceira variação une o que há de melhor nas primeira e segunda varições ele é o algoritmo mais eficiente.
-}

{-
    Questão 3
-}

{- Variação 1 -}

minimo :: Ord t => [t] -> t
minimo [] = undefined
minimo [x] = x
minimo (x : r)
    | x <= minimo r = x
    | otherwise = minimo r

selecao3V1 :: Ord t => [t] -> [t]
selecao3V1 [] = []
selecao3V1 l = x : selecao3V1 (remove x l)
    where 
        x = minimo l

{- Variação 2 -}

aux_concat2 :: t -> (t, [t]) -> (t, [t])
aux_concat2 j (x, l) = (x, j : l)

remove_menor3V2 :: Ord t => [t] -> (t, [t])
remove_menor3V2 [] = undefined
remove_menor3V2 [x] = (x, [])
remove_menor3V2 (x : r)
    | x <= menor = (x, r)
    | otherwise = aux_concat2 x (menor, lista)
    where
        (menor, lista) = remove_menor3V2 r

selecao3V2 :: Ord t => [t] -> [t]
selecao3V2 [] = []
selecao3V2 l = x : selecao3V2 (nl)
    where 
        (x, nl) = remove_menor3V2 l

{- Variação 2 com contador -}

aux_concat3 :: t -> (t, [t], Int) -> (t, [t], Int)
aux_concat3 j (x, l, k) = (x, j : l, k)

aux_concat4 :: t -> ([t],b) -> ([t],b)
aux_concat4 x (y,k) = (x:y,k)

remove_menor3V2C :: Ord t => ([t],Int) -> (t,[t],Int)
remove_menor3V2C ([],_) = undefined
remove_menor3V2C ([x],k) = (x,[],k)
remove_menor3V2C (x:r,k)
    | x <= menor = (x,r,k2+1)
    | otherwise = aux_concat3 x (menor,l,k2+1)
    where 
        (menor,l,k2) = remove_menor3V2C (r,k)

select3V2C :: Ord t => ([t],Int) -> ([t],Int)
select3V2C ([],k) = ([],k)
select3V2C (l,k) = aux_concat4 m (select3V2C (l2,k2))
    where 
        (m,l2,k2) = remove_menor3V2C (l,k)

selecao3V2C :: Ord t => [t] -> ([t],Int)
selecao3V2C [] = ([],0)
selecao3V2C l = select3V2C (l,0)        

{-
    Comparações 
    A segunda variação tem vantagem sobre listas completamente deordenadas e, em seu pior caso, invertidas. Otimizamos o algoritmo de busca do menor elemento que, ao contrário do algoritmo original, não fica se chamando recursivamente quase pela eternidade.
    O algoritmo original faz o número máximo de comparações e, devido a pouca ou nenhuma otimização, não consegue ordenar listas completamente desordenadas com 100 elementos ou mais.
-}

{- 
    Melhor algoritmo
    Como a segunda variação otimiza as chamadas recursivas e une as funções que procura o menor elemento e o remove em uma única função ele é o algoritmo mais eficiente.
-}

{-
    Questão 4
-}

{- Variação 1 -}

concat_fst4V1 :: t -> ([t], [t]) -> ([t], [t])
concat_fst4V1 v (l1, l2) = (v : l1, l2)

concat_snd4V1 :: t -> ([t], [t]) -> ([t], [t])
concat_snd4V1 v (l1, l2) = (l1, v : l2)

divide4V1 :: Ord t => t -> [t] -> ([t], [t])
divide4V1 n [] = ([], [])
divide4V1 n (h : t)
    | h < n = concat_fst4V1 h (divide4V1 n t)
    | otherwise = concat_snd4V1 h (divide4V1 n t)

rapido4V1 :: Ord t => [t] -> [t]
rapido4V1 [] = []
rapido4V1 (p : r) = rapido4V1 menores ++ [p] ++ rapido4V1 maiores
    where 
        (menores, maiores) = divide4V1 p r

{- Variação 2 -}

escolhe_pivo4V2 :: Ord t => [t] -> (t, [t])
escolhe_pivo4V2 [x] = (x, [])
escolhe_pivo4V2 [x, y] = (x, [y])
escolhe_pivo4V2 (x : y : z : r)
    | (y <= x && x <= z) || (z <= x && x <= y) = (x, y : z : r)
    | (x <= y && y <= z) || (z <= y && y <= x) = (y, x : z : r)
    | otherwise = (z, x : y : r)

rapido4V2 :: Ord t => [t] -> [t]
rapido4V2 [] = []
rapido4V2 l = rapido4V2 menores ++ [p] ++ rapido4V2 maiores
    where
        (p, r) = escolhe_pivo4V2 l
        (menores, maiores) = divide4V1 p r

{- Variação 1 com contador -}

concat_fst :: t -> ([t], [t], Int) -> ([t], [t], Int)
concat_fst v (l1, l2, k) = (v : l1, l2, k)

concat_snd :: t -> ([t], [t], Int) -> ([t], [t], Int)
concat_snd v (l1, l2, k) = (l1, v : l2, k)

divide4V1C :: Ord t => t -> ([t], Int) -> ([t], [t], Int)
divide4V1C n ([], k) = ([], [], k)
divide4V1C n (h : t, k)
    | h < n = (nl1, l2, nk1 + 1)
    | otherwise = (l1, nl2, nk2 + 1)
    where
        (nl1, l2, nk1) = concat_fst h (divide4V1C n (t, k))
        (l1, nl2, nk2) = concat_snd h (divide4V1C n (t, k))

rapido4V1C :: Ord t => [t] -> ([t], Int)
rapido4V1C [] = ([], 0)
rapido4V1C (p : r) =  (l, c)
    where 
        (menores, maiores, cont) = divide4V1C p (r, 0)
        (men, k1) = rapido4V1C menores
        (mai, k2) = rapido4V1C maiores
        (l, c) = (men ++ [p] ++ mai, k1 + k2 + cont)

{- Variação 2 com contador -}

aux_concat5 :: Int -> ([a],Int) -> ([a],Int) -> ([a],Int)
aux_concat5 k3 (x,k1) (y,k2) = (x ++ y, k1 + k2 + k3)

divide4V2C :: Ord t => t -> [t] -> Int -> ([t],[t],Int)
divide4V2C n [] k = ([],[],k)
divide4V2C n (h : t) k
    | h < n = concat_fst h (divide4V2C n t (k+1))
    | otherwise = concat_snd h (divide4V2C n t (k+1))

escolhe_pivo4V2C :: Ord t => ([t],Int) -> (t,[t],Int)
escolhe_pivo4V2C ([x],k) = (x,[],k)
escolhe_pivo4V2C ([x,y],k) = (x,[y],k)
escolhe_pivo4V2C (x:y:z:r,k)
    | (y <= x && x <= z) || (z <= x && x <= y) = (x,y:z:r,k+4)
    | (x <= y && y <= z) || (z <= y && y <= x) = (y,x:z:r,k+8)
    | otherwise = (z,x:y:r,k+8)

quick4V2C :: Ord t => ([t],Int) -> ([t],Int)
quick4V2C ([],k) = ([],k)
quick4V2C (l,k) = aux_concat5 k3 (quick4V2C (menores,0)) (junta p (quick4V2C (maiores,0)))
    where
       (p,r,k2) = escolhe_pivo4V2C (l,k)
       (menores, maiores, k3) = divide4V2C p r k2

rapido4V2C :: Ord t => [t] -> ([t],Int)
rapido4V2C [] = ([],0)
rapido4V2C l = quick4V2C (l,0)

{- 
    Comparações 
    A primeira variação já é melhor otimizada que o algoritmo original, pois ela percorre uma única vez a lista a cara iteração para dividi-la nas listas com os menores e maiores elementos que o pivô.
    A segunda variação une a atimização da primeira variação com uma melhor escolha de pivô que, apesar de adicionar 4 ou 8 comparações na contagem tende a ajudar separar melhor as listas e, por consequência, diminuir o número de iterações para separar todos os elementos.
    O algoritmo original faz o número máximo de comparações devido a falta de otimização.
-}

{-
    Melhor algoritmo
    Como a segunda variação otimiza as chamadas recursivas e escolhe melho seu pivô ele é o algoritmo mais eficiente.
-}

{-
    Questão 5
-}

{- MergeSort -}

merge5V1 :: [Int] -> [Int] -> [Int]
merge5V1 [] l2 = l2
merge5V1 l1 [] = l1
merge5V1 (h1 : t1) (h2 : t2)
    | h1 < h2 = h1 : merge5V1 t1 (h2 : t2)
    | otherwise = h2 : merge5V1 (h1 : t1) t2

mergesortaux5V1 :: [Int] -> Int -> Int -> [Int]
mergesortaux5V1 l comeco fim
    | comeco == fim = [l !! comeco]
    | comeco < fim = merge5V1 (mergesortaux5V1 l comeco meio) (mergesortaux5V1 l (meio + 1) fim)
    | otherwise = []
    where
        meio = div (fim + comeco) 2

mergesort5V1 :: [Int] -> [Int]
mergesort5V1 l = mergesortaux5V1 l 0 (length l - 1)

{- BucketSort -}



{- MergeSort com contador -}

merge5V1C :: ([Int], Int) -> ([Int], Int) -> ([Int], Int)
merge5V1C ([], k1) (l2, k2) = (l2, k2 + k1) -- Só k2?
merge5V1C (l1, k1) ([], k2) = (l1, k1 + k2) -- Só k1?
merge5V1C (h1 : t1, k1) (h2 : t2, k2)
    | h1 < h2 = aux_concat4 h1 (merge5V1C (t1, k1 + 1) (h2 : t2, k2))
    | otherwise = aux_concat4 h2 (merge5V1C (h1 : t1, k1) (t2, k2 + 1))

mergesortaux5V1C :: ([Int], Int) -> Int -> Int -> ([Int], Int)
mergesortaux5V1C (l, k) comeco fim
    | comeco == fim = ([l !! comeco], k + 1)
    | comeco < fim = merge5V1C (mergesortaux5V1C (l, k) comeco meio) (mergesortaux5V1C (l, 2) (meio + 1) fim)
    | otherwise = ([], k)
    where
        meio = div (fim + comeco) 2

mergesort5V1C :: [Int] -> ([Int], Int)
mergesort5V1C l = mergesortaux5V1C (l, 0) 0 (length l - 1)

{- BucketSort com contador -}



{- Comparações -}



{-
    Questão 6
-}

{- Letra A -}

data Exp a =
    Val Int -- Um número
    | Add (Exp a) (Exp a) -- soma de duas expressões
    | Sub (Exp a) (Exp a) -- subtração
    | Mul (Exp a) (Exp a) -- multiplicação
    | Pot (Exp a) (Exp a) -- potenciação

avalia:: Exp a -> Int
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ^ (avalia exp2)

{- Letra B -}

e1 :: Exp a
e1 = Mul (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mul (Val 1) (Val 3)))

-- avalia e1 retorna 15000

e2 :: Exp a
e2 = Sub (Val 0) (Mul (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Pot (Val 6) (Val 2))))

-- avalia e2 retorna -380

{-
    Questão 7
-}

data Hora = AM Int Int
          | PM Int Int
          deriving (Show, Eq, Ord)
          
{- Letra A -}

horasDecorridas :: Hora -> Int
horasDecorridas (AM x _) = x
horasDecorridas (PM x _) = x + 12

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM x y) = x * 60 + y 
minutosDecorridos (PM x y) = (x + 12) * 60 + y

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM x y) = (x * 60 + y) * 60
segundosDecorridos (PM x y) = ((x + 12) * 60 + y) * 60

{- Letra B -}

horasDecorridas2 :: Hora -> Int
horasDecorridas2 (AM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                      else x
horasDecorridas2 (PM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                      else x + 12

minutosDecorridos2 :: Hora -> Int
minutosDecorridos2 (AM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                        else x * 60 + y 
minutosDecorridos2 (PM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                        else (x + 12) * 60 + y

segundosDecorridos2 :: Hora -> Int
segundosDecorridos2 (AM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                         else (x * 60 + y) * 60
segundosDecorridos2 (PM x y) = if (x < 0 || x > 11) || (y < 0 || y > 59) then undefined
                                                                         else ((x + 12) * 60 + y) * 60
                                                                         
{- Letra C -}

-- Ao utilizar "Eq e Ord" dentro de "deriving ()" na definição do tipo Hora, podemos fazer essas comparações.

{-
    Questão 8
-}


{- Letra A -}

type Data = (Int, Int, Int)

-- Tipo Hora definido na questão 7 

data App = LinkedIn | WhatsApp | Facebook
           deriving (Show, Eq, Ord)

data Mensagem = Fone String String Data Hora App
              | Nome String String Data Hora App
               deriving (Show, Eq, Ord)

m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30 :: Mensagem

m1 = Nome "Augusto Costa" "A apresentacao de 13h foi cancelada" (26, 9, 2020) (AM 10 30) WhatsApp
m2 = Fone "1122-3344" "Alguma coisa" (26, 9, 2020) (AM 10 50) WhatsApp
m3 = Nome "Augusto Costa" "Outra coisa" (27, 9, 2020) (PM 3 30) Facebook
m4 = Nome "Augusto Costa" "Mensagem 4" (26, 9, 2020) (AM 11 10) LinkedIn
m5 = Fone "3344-4455" "Mensagem 5" (26, 9, 2020) (AM 10 30) WhatsApp
m6 = Fone "3344-4455" "Mensagem 6" (27, 9, 2020) (AM 10 30) LinkedIn
m7 = Fone "1122-3344" "Mensagem 7" (27, 9, 2020) (PM 4 22) Facebook
m8 = Nome "Augusto Moraes" "Muitas coisas" (26, 9, 2020) (AM 7 10) LinkedIn
m9 = Fone "1122-3344" "A Ana ta dodoi" (27, 9, 2020) (AM 9 40) WhatsApp
m10 = Nome "Faustao" "Ta pegando fogo bixo" (26, 9, 2020) (AM 10 10) WhatsApp
m11 = Nome "Louro Jose" "Procura-se novo emprego" (26, 9, 2020) (AM 8 50) LinkedIn
m12 = Nome "Sivio Santos" "Quem quer um aviaozinho?" (26, 9, 2020) (AM 6 25) WhatsApp
m13 = Nome "Patricia Abravanel" "Sabe que horas sao papai?" (26, 9, 2020) (AM 6 34) WhatsApp
m14 = Nome "Silvio Santos" "Hora do aviaozinho?" (27, 9, 2020) (AM 6 42) Facebook
m15 = Nome "Celso Portinhola" "A gincana de hoje foi cancelada" (26, 9, 2020) (AM 11 56) WhatsApp
m16 = Nome "Fatima Bernardes" "TV Globinho chorou pra mim" (26, 9, 2020) (AM 8 10) LinkedIn
m17 = Fone "3344-4455" "Procura-se um novo emprego pra chamar de meu" (27, 9, 2020) (AM 3 40) LinkedIn
m18 = Nome "Bonner" "TV Globinho volta com tudo a partir da semana que vem" (26, 9, 2020) (AM 8 12) WhatsApp
m19 = Nome "Chaves" "A vinganca nunca e plena Bonner" (26, 9, 2020) (AM 8 15) WhatsApp
m20 = Nome "Bonner" "Abaixo assinado para a volta da TV Globinho" (27, 9, 2020) (AM 8 17) Facebook
m21 = Nome "Louro Jose" "Quem vive no abacaxi e mora no mar?" (27, 9, 2020) (PM 3 12) Facebook
m22 = Nome "Faustao" "O loko meu" (27, 9, 2020) (PM 2 07) LinkedIn
m23 = Nome "Silvio Santos" "Quem quer dinheiro!?!?!?" (26, 9, 2020) (PM 1 10) LinkedIn
m24 = Nome "Chaves" "Que roupinha mutcho loka" (27, 9, 2020) (PM 4 16) Facebook
m25 = Nome "Patricia Abravanel" "Procura-se hospital psiquiatrico" (27, 9, 2020) (PM 5 54) LinkedIn
m26 = Nome "Celso Portinhola" "Meu programa, minha vida" (27, 9, 2020) (PM 6 08) LinkedIn
m27 = Nome "Eliana" "A Xuxa nao e nada perto do que eu fui" (26, 9, 2020) (AM 11 45) WhatsApp
m28 = Nome "Ana Maria Braga" "Procura-se novo papagaio cenografico" (26, 9, 2020) (PM 9 20) LinkedIn
m29 = Nome "Ana Maria Braga" "Acredita que o Louro Jose tentou comer meu cabelo?" (26, 9, 2020) (AM 10 30) WhatsApp
m30 = Nome "Ana Maria Braga" "Amo os animais" (27, 9, 2020) (AM 10 30) LinkedIn

{- Letra B -}

lm:: [Mensagem]
lm = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30]

removeUlt8V1 :: [a] -> ([a],a)
removeUlt8V1 [x] = ([],x)
removeUlt8V1 (x:y) = junta x (removeUlt8V1 y)

comparaContato :: Mensagem -> Mensagem -> Bool
comparaContato (Nome _ _ _ _ _) (Fone _ _ _ _ _) = True
comparaContato (Fone _ _ _ _ _) (Nome _ _ _ _ _) = False
comparaContato (Nome n1 _ _ _ _) (Nome n2 _ _ _ _) = if n1 > n2 then True else False
comparaContato (Fone n1 _ _ _ _) (Fone n2 _ _ _ _) = if n1 > n2 then True else False

troca8V1 :: ([Mensagem],Int) -> ([Mensagem],Int)
troca8V1 ([x],c) = ([x],c)
troca8V1 (x:y:z,c)
     | comparaContato x y = junta y (troca8V1 (x:z,c+1)) 
     | otherwise = junta x (troca8V1 (y:z,c))

bolhaOrd8V1 :: [Mensagem] -> [Mensagem]
bolhaOrd8V1 lista = if x == 0 then lista
                            else (bolhaOrd8V1 ls) ++ [u]
           where
              (l,x) = troca8V1 (lista,0)
              (ls,u) = removeUlt8V1 l

bolha8V1 :: [Mensagem] -> [Mensagem]
bolha8V1 [] = []
bolha8V1 lista = bolhaOrd8V1 lista

{- Letra C -}

diaAntOuMsm :: Data -> Data -> Bool
diaAntOuMsm (d1,m1,a1) (d2,m2,a2)
     | d1 <= d2 = True
     | d1 > d2 = False

comparaDataEHora :: Mensagem -> Mensagem -> Mensagem -> Bool
comparaDataEHora (Fone _ _ dx hx _) (Fone _ _ dy hy _) (Fone _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Nome _ _ dx hx _) (Fone _ _ dy hy _) (Fone _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Fone _ _ dx hx _) (Nome _ _ dy hy _) (Fone _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Fone _ _ dx hx _) (Fone _ _ dy hy _) (Nome _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Nome _ _ dx hx _) (Nome _ _ dy hy _) (Fone _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Nome _ _ dx hx _) (Fone _ _ dy hy _) (Nome _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Fone _ _ dx hx _) (Nome _ _ dy hy _) (Nome _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)
comparaDataEHora (Nome _ _ dx hx _) (Nome _ _ dy hy _) (Nome _ _ dz hz _) = if (diaAntOuMsm dy dx && diaAntOuMsm dx dz) then True else (hy <= hx && hx <= hz) || if (diaAntOuMsm dz dx && diaAntOuMsm dx dy) then True else (hz <= hx && hx <= hy)

escolhe_pivo8V1 :: [Mensagem] -> (Mensagem, [Mensagem])
escolhe_pivo8V1 [x] = (x, [])
escolhe_pivo8V1 [x, y] = (x, [y])
escolhe_pivo8V1 (x : y : z : r)
    | comparaDataEHora x y z = (x, y : z : r)
    | comparaDataEHora y x z = (y, x : z : r)
    | otherwise = (z, x : y : r)

rapido8V1 :: [Mensagem] -> [Mensagem]
rapido8V1 [] = []
rapido8V1 l = rapido8V1 menores ++ [p] ++ rapido8V1 maiores
    where
        (p, r) = escolhe_pivo8V1 l
        (menores, maiores) = divide4V1 p r

{- Letra D -}



{-
    Questão 9
-}

data ArvoreBinInt = Nulo 
                  | No Int ArvoreBinInt ArvoreBinInt
                      deriving Show

arvExemplo :: ArvoreBinInt
arvExemplo = (No 2 (No 7 (No 2 Nulo Nulo)
                    (No 6 (No 5 Nulo Nulo)
                          (No 11 Nulo Nulo)))
              (No 5 Nulo
                    (No 9 (No 4 Nulo Nulo)
                          Nulo)))

{- Letra A -}

internos :: ArvoreBinInt -> [Int]
internos Nulo = []
internos (No x Nulo Nulo) = []
internos (No x esq dir) = (internos esq) ++ [x] ++ (internos dir)

{- Letra B -}

somaNos :: ArvoreBinInt -> Int
somaNos Nulo = 0
somaNos (No x esq dir) = (somaNos esq) + x + (somaNos dir)

{- Letra C -}

pertence :: Int -> ArvoreBinInt -> Bool
pertence _ Nulo = False
pertence elem (No x esq dir) = (pertence elem esq) || x == elem || (pertence elem dir)

{-
    Questão 10
-}

data ArvBinEA a = Vazia
                | Folha a
                | NoEA (Char, ArvBinEA a, ArvBinEA a)
                   deriving Show

eaExemplo :: ArvBinEA Float
eaExemplo = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

resolve_ea :: ArvBinEA Float -> Float
resolve_ea Vazia = 0
resolve_ea (Folha a) = a
resolve_ea (NoEA (c, esq, dir))
           | c == '+' = (resolve_ea esq) + (resolve_ea dir)
           | c == '-' = (resolve_ea esq) - (resolve_ea dir)
           | c == '*' = (resolve_ea esq) * (resolve_ea dir)
           | c == '/' = (resolve_ea esq) / (resolve_ea dir)
           | c == '^' = (resolve_ea esq) ** (resolve_ea dir)
           | otherwise = undefined
