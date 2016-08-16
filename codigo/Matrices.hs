-- Matrices y grafos
-- =====================================================================

module Matrices where

import GrafoConListaDeAristas   ( Grafo
                                , aristaEn
                                , creaGrafo
                                )
import EjemplosGrafos           ( grafoCiclo
                                , completo
                                , grafoPetersen
                                )
import DefinicionesYPropiedades ( orden
                                , tamaño
                                )
import Data.Matrix              ( Matrix
                                , fromLists
                                , matrix
                                , toList
                                , toLists
                                , transpose
                                )
import Test.QuickCheck          ( Gen
                                , Property
                                , choose
                                , forAll
                                , quickCheck
                                , sublistOf
                                )

-- | (imprimeMatriz p) imprime la matriz p con una fila por línea. Por
-- ejemplo, 
-- >>> imprimeMatrix (fromLists [[1,3,4],[3,5,7],[4,7,9]])
-- [1,3,4]
-- [3,5,7]
-- [4,7,9]
imprimeMatrix :: Show a => Matrix a -> IO ()
imprimeMatrix p =
  mapM_ print (toLists p)

-- | (matrizAdyacencia g) es la matriz de adyacencia del grafo g. Por
-- ejemplo,
-- >>> imprimeMatrix (matrizAdyacencia (grafoCiclo 4))
-- [0,1,0,1]
-- [1,0,1,0]
-- [0,1,0,1]
-- [1,0,1,0]
-- >>> imprimeMatrix (matrizAdyacencia (completo 4))
-- [0,1,1,1]
-- [1,0,1,1]
-- [1,1,0,1]
-- [1,1,1,0]
matrizAdyacencia :: Grafo Int -> Matrix Int
matrizAdyacencia g = matrix n n f
  where n = orden g
        f (i,j) | (i,j) `aristaEn`g = 1
                | otherwise         = 0

-- | (esSimetrica p) se verifica si la matriz p es simétrica. Por
-- ejemplo, 
-- >>> esSimetrica (fromLists [[1,3,4],[3,5,7],[4,7,9]])
-- True
-- >>> esSimetrica (fromLists [[1,3,4],[3,5,7],[4,9.7]])
-- False
esSimetrica :: Eq a => Matrix a -> Bool
esSimetrica p =
  transpose p == p

-- Prop. Para todo n, la matriz de adyacencia del grafo ciclo de orden n
-- es simétrica.
prop_simetricaAdyacenciaCiclo :: Int -> Bool
prop_simetricaAdyacenciaCiclo n =
  esSimetrica (matrizAdyacencia (grafoCiclo n))

-- Comprobación para n <= 30
--    ghci> all prop_simetricaAdyacenciaCiclo [1..30]
--    True

-- Prop. Para todo n, la matriz de adyacencia del grafo completo de orden n
-- es simétrica.
prop_simetricaAdyacenciaCompleto :: Int -> Bool
prop_simetricaAdyacenciaCompleto n =
  esSimetrica (matrizAdyacencia (completo n))

-- Comprobación para n <= 30
--    ghci> all prop_simetricaAdyacenciaCompleto [1..30]
--    True

-- Generador de grafos simples
grafoSimple :: Gen (Grafo Int)
grafoSimple = do
  n  <- choose (0,10)
  as <- sublistOf [(x,y) | x <- [1..n], y <- [x+1..n]] 
  return (creaGrafo [1..n] as)

-- Ejemplos
--    ghci> sample grafoSimple
--    G [1,2] [(1,2)]
--    G [1,2,3,4] [(1,2),(1,3),(1,4),(2,4),(3,4)]
--    G [1,2,3] [(2,3)]
--    G [1,2,3,4,5] [(1,2),(1,3),(1,5),(2,4),(3,4),(3,5)]
--    G [1,2,3,4] [(1,3),(1,4),(2,4)]
--    G [1] []
--    G [1,2,3] [(1,2),(1,3)]


-- | (tamañoM g) es el tamaño del grafo simple g, calculado usando su matriz de
-- adyacencia. Por ejemplo,
-- >>> tamañoM (grafoCiclo 4)
-- 4
-- >>> tamañoM grafoPetersen
-- 15
tamañoM :: Grafo Int -> Int
tamañoM g = sum (toList (matrizAdyacencia g)) `div` 2

-- Prop. El tamaño de un grafo simple es la mitad de la suma de los
-- elementos de su matriz de adyacencia. 
prop_tamaño :: Property
prop_tamaño =
  forAll grafoSimple  
         (\g -> tamaño g == tamañoM g)

-- La comprobación es
--    ghci> quickCheck prop_tamaño
--    +++ OK, passed 100 tests.



  
