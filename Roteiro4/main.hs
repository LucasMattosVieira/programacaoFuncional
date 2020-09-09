--Exercício 2

quadrados :: Int -> Int -> [Int]
quadrados a b = [ x^2 | x <- [a..b] ]

--Exercício 3

seleciona_impares :: [Int] -> [Int]
seleciona_impares l = [ x | x <- l, mod x 2 /= 0 ]

--Exercício 4

tabuada :: Int -> [Int]
tabuada n = [ x*n | x <- [1..10] ]

--Exercício 5

bissexto :: Int -> Bool
bissexto a
     | (mod a 400 == 0) = True
     | (mod a 4 == 0) && (mod a 100 /= 0) = True
     | otherwise = False

bissextos :: [Int] -> [Int]
bissextos l = [ x | x <- l, bissexto x ]

--Exercício 6

sublistas :: [[t]] -> [t]
sublistas l = [ x | y <- l, x <- y ]

--Exercício 7

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
  ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
  ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d1,m1,a1) (d2,m2,a2)
     | a1 < a2 = True
     | a1 > a2 = False
     | m1 < m2 = True
     | m1 > m2 = False
     | d1 < d2 = True
     | d1 >= d2 = False

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados l dataAtual = [ (cl,ca,de,dd,s) | (cl,ca,de,dd,s) <- l, precede dd dataAtual, s == "aberto" ]

--Exercício 8

npares :: [Int] -> Int
npares [] = 0
npares (h:t) = if mod h 2 == 0 then 1 + npares t
                               else npares t

--Exercício 9

produtorio :: [Float] -> Float
produtorio [] = 0
produtorio [x] = x
produtorio (h:t) = h * produtorio t

--Exercício 10

comprime :: [[t]] -> [t]
comprime [] = []
comprime ([]:r) = comprime r
comprime ((x:y):r) = x : comprime ((y):r)

--Exercício 11

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (h:t) = 1 + tamanho t

--Exercício 12

uniaoNRec :: Eq t => [t] -> [t] -> [t]
uniaoNRec a b = a ++ [ x | x <- b, not (elem x a) ]

--Exercício 13

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 l [] = l
uniaoRec2 l (x:xs) = if elem x l then uniaoRec2 l xs
                                 else uniaoRec2 (l ++ [x]) xs

