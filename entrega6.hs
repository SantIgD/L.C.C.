import Seq
import ListSeq
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
