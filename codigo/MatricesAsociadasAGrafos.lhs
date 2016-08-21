\ignora{
\begin{code}
module MatricesAsociadasAGrafos where

import GeneradorGrafosSimples   ( grafoSimple
                                )
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
\end{code}
}

La función \texttt{(imprimeMatriz p)} imprime por pantalla la matriz
\texttt{p} con una fila por línea.

\index{\texttt{imprimeMatriz}}
\begin{code}
-- | Ejemplo 
-- >>> imprimeMatriz (fromLists [[1,3,4],[3,5,7],[4,7,9]])
-- [1,3,4]
-- [3,5,7]
-- [4,7,9]
imprimeMatriz :: Show a => Matrix a -> IO ()
imprimeMatriz p =
  mapM_ print (toLists p)
\end{code}

La función \texttt{(matrizAdyacencia g)} es la matriz de adyacencia 
del grafo \texttt{g}.

\index{\texttt{matrizAdyacencia}}
\begin{code}
-- | Ejemplo,
-- >>> imprimeMatriz (matrizAdyacencia (grafoCiclo 4))
-- [0,1,0,1]
-- [1,0,1,0]
-- [0,1,0,1]
-- [1,0,1,0]
-- >>> imprimeMatriz (matrizAdyacencia (completo 4))
-- [0,1,1,1]
-- [1,0,1,1]
-- [1,1,0,1]
-- [1,1,1,0]
matrizAdyacencia :: Grafo Int -> Matrix Int
matrizAdyacencia g = matrix n n f
  where n = orden g
        f (i,j) | (i,j) `aristaEn` g = 1
                | otherwise          = 0
\end{code}

La función \texttt{(esSimetrica p)} se verifica si la matriz \texttt{p}
es simétrica.

\index{\texttt{esSimetrica}}
\begin{code}
-- ejemplo, 
-- >>> esSimetrica (fromLists [[1,3,4],[3,5,7],[4,7,9]])
-- True
-- >>> esSimetrica (fromLists [[1,3,4],[3,5,7],[4,9.7]])
-- False
esSimetrica :: Eq a => Matrix a -> Bool
esSimetrica p =
  transpose p == p
\end{code}

\begin{teorema}
  Para todo $n$, la matriz de adyacencia del grafo ciclo de orden $n$,
  $C_n$ es simétrica.
\end{teorema}

La comprobación del teorema para $n \leq 30$ es:

\begin{sesion}
ghci> all prop_simetricaAdyacenciaCiclo [1..30]
True
\end{sesion}

\index{\texttt{prop_simetricaAdyacenciaCiclo}}
\begin{code}
prop_simetricaAdyacenciaCiclo :: Int -> Bool
prop_simetricaAdyacenciaCiclo n =
  esSimetrica (matrizAdyacencia (grafoCiclo n))
\end{code}

\begin{teorema}
  Para todo $n$, la matriz de adyacencia del grafo completo de orden $n$,
  $C_n$ es simétrica.
\end{teorema}

\begin{sesion}
ghci> all prop_simetricaAdyacenciaCompleto [1..30]
True
\end{sesion}

\index{\texttt{prop_simetricaAdyacenciaCompleto}}
\begin{code}
prop_simetricaAdyacenciaCompleto :: Int -> Bool
prop_simetricaAdyacenciaCompleto n =
  esSimetrica (matrizAdyacencia (completo n))
\end{code}

La función \texttt{(tamañoM g)} devuelve el tamaño del grafo simple 
\texttt{g}, calculado usando su matriz de adyacencia. 

\index{\texttt{tamañoM}}
\begin{code}
-- | Ejemplos
-- >>> tamañoM (grafoCiclo 4)
-- 4
-- >>> tamañoM grafoPetersen
-- 15
tamañoM :: Grafo Int -> Int
tamañoM g = sum (toList (matrizAdyacencia g)) `div` 2
\end{code}

\begin{teorema}
  El tamaño de un grafo simple es la mitad de la suma de los 
  elementos de su matriz de adyacencia.
\end{teorema}

La comprobación del teorema con QuickCheck es:

\begin{sesion}
ghci> quickCheck prop_tamaño
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop_tamaño}}
\begin{code}
prop_tamaño :: Property
prop_tamaño =
  forAll grafoSimple  
         (\g -> tamaño g == tamañoM g)
\end{code}



  
