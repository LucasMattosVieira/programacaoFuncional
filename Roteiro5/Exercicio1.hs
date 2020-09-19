--a)
type Data = (Int,Int,Int)

valida :: Data -> Bool
valida (d,m,a)
     | (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) && d >= 1 && d <= 31 = True
     | (m == 4 || m == 6 || m == 9 || m == 11) && d >= 1 && d <= 30 = True
     | (bissexto a) && m == 2 && d >= 1 && d <= 29 = True
     | not (bissexto a) && m == 2 && d >= 1 && d <= 28 = True
     | otherwise = False
     where
     bissexto a 
          | (mod a 400 == 0) = True
          | (mod a 4 == 0) && (mod a 100 /= 0) = True
          | otherwise = False

--b)
bissextos :: [Int] -> [Int]
bissextos l = [ a | a <- l, bissexto a ]
     where
     bissexto a 
          | (mod a 400 == 0) = True
          | (mod a 4 == 0) && (mod a 100 /= 0) = True
          | otherwise = False

--c)
type Emprestimo = (String,String,Data,Data,String)
type Emprestimos = [Emprestimo]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados l hoje = [ e | e <- l, verify e hoje == "atrasado"]
      where 
      verify (cl,ca,de,dd,s) hoje = if (precede dd hoje) && (s == "aberto") then "atrasado"
                                                                            else "em dia"
      precede (d1,m1,a1) (d2,m2,a2)
           | a1 < a2 = True
           | a1 > a2 = False
           | m1 < m2 = True
           | m1 > m2 = False
           | d1 < d2 = True
           | d1 >= d2 = False

--d)
fibo2 :: Int -> Int
fibo2 n = fst(fibo n)
     where
     fibo 0 = (0,1)
     fibo k = passo (fibo (k-1))
     passo (x,y) = (y,x+y)

--e)
fatorial :: Int -> Int
fatorial n = fat n
     where
     fat 0 = 1
     fat k = prodIntervalo 1 k
     prodIntervalo m n = if m == n then m
                                   else n * prodIntervalo m (n-1)

