import Seq
import ArrSeq
import Par ((|||))
-- Practica 6 --

-- Ejercicio 4

--b)

data Paren = Open | Close 

matchParen :: Seq s => s Paren -> Bool
matchParen seqParen = bienAnidados (scanS (+) 0 (mapS f seqParen))


bienAnidados :: Seq s => (s Int,Int) -> Bool
bienAnidados (xs,0) = menosClose  (map (\x -> x >= 0) xs)
bienAnidados _      = False


f :: Paren -> Int
f Close = (-1)
f _     = 1

menosClose :: Seq s => s Int -> Bool
menosClose seqInt = reduceS (&&) True seqInt


-- Ejercicio 6

multiplos seqParen = reduceS (+) 0 
                                (tabulateS
                                  (\i -> let subSeq = dropS seqParen i 
                                         in reduceS (+) 0 (map (k (nthS subSeq 0) xs) (dropS subSeq 1))
                                  ) 
                                    l
                                ) seqParen
                 where 
                   l = (length seqParen) - 1
                
k :: Int -> Int -> Int
k x y = if mod x y == 0 then 1 else 0
