import Seq
import ArrSeq
import Par ((|||))
import qualified Arr as A
-- Practica 6 --

-- Ejercicio 4

--b)

data Paren = Open | Close 

matchParen :: Seq s => s Paren -> Bool
matchParen seqParen = bienAnidados (scanS (+) 0 (mapS f seqParen))


bienAnidados :: Seq s => (s Int,Int) -> Bool
bienAnidados (xs,0) = menosClose  (mapS (\x -> x >= 0) xs)
bienAnidados _      = False


f :: Paren -> Int
f Close = (-1)
f _     = 1

menosClose :: Seq s => s Bool -> Bool
menosClose seqInt = reduceS (&&) True seqInt

parentesis1 = [Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close]
parentesis2 = [Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close]
parentesis3 = [Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Open,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close,Close]

-- Ejercicio 6
multiplos :: Seq s => s Int -> Int
multiplos seqInt = reduceS (+) 0 (seqCantidadMultiplos seqInt)
                 
                
seqCantidadMultiplos :: Seq s => s Int -> s Int
seqCantidadMultiplos seqInt = tabulateS (\i -> multiplosDeElemento (dropS seqInt i)) l
                             where 
                               l = (lengthS seqInt) - 1

multiplosDeElemento :: Seq s => s Int -> Int
multiplosDeElemento seqInt = reduceS (+) 0 (mapS (k (nthS seqInt 0)) (dropS seqInt 1))


k :: Int -> Int -> Int
k x y = if mod x y == 0 then 1 else 0




