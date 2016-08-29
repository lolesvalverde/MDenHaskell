\ignora{
\begin{code}
module MatricesDeAdyacencia where

import GeneradorGrafosSimples   ( grafoSimple
                                )
import GrafoConListaDeAristas   ( Grafo
                                , aristaEn
                                , creaGrafo
                                , vertices
                                )
import EjemplosGrafos           ( grafoCiclo
                                , completo
                                , grafoPetersen
                                , esBipartito
                                , conjuntosVerticesDisjuntos
                                )
import DefinicionesYPropiedades ( orden
                                , tamaño
                                , esAislado
                                , grado
                                )
import Data.Matrix              ( Matrix
                                , fromLists
                                , matrix
                                , toList
                                , toLists
                                , transpose
                                , getElem
                                )
import Data.List                ( subsequences
                                )
import Data.Maybe               ( fromJust)
import Test.QuickCheck          ( Gen
                                , Property
                                , choose
                                , forAll
                                , quickCheck
                                , sublistOf
                                , (==>)
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

\begin{definicion}
  Sea $G=(V,A)$ un grafo simple con $V=\{v_1, \dots, v_n\}$. Se define su  
  matriz de adyacencia como $\mathcal{A}_{n \times n} = (a_{i,j})$ donde  
  el elemento $a_{i,j}$ toma el valor $1$ si existe alguna arista en $A$
  uniendo los vértices $v_i$ y $v_j$ y $0$ en caso contrario.
\end{definicion} 

\begin{nota}
  La matriz de adyacencia depende del etiquetado del grafo.
\end{nota}

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

\begin{teorema}
  Si un grafo $G=(V,A)$ tiene un vértice aislado $v_i$, tanto la fila como la 
  columna $i-$ésima de su matriz de adyacencia estarán formadas por ceros.
\end{teorema}

La comprobación del teorema es:

\begin{sesion}
ghci> quickCheck prop_verticeAislado
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop_verticeAislado}}
\begin{code}
prop_verticeAislado :: Grafo Int -> Bool
prop_verticeAislado g = 
    all p (filter (esAislado g) (vertices g))
        where p v = all (==0) ((toLists (ma g)) !! (v-1))
              ma = matrizAdyacencia
\end{code}

\begin{teorema}
  Sea un grafo $G=(V,A)$ no dirigido con $V=\{v_1, \dots, v_n\}$. La  
  suma de los elementos de la fila (o columna) $i-$ésima de su matriz 
  de adyacencia coincide con el grado del vértice $v_i$. Es decir,

\begin{equation*}
\delta(v_i) = \sum_{j=1}^{n} a_{i,j} = \sum_{j=1}^{n} a_{j,i} 
\end{equation*}
\end{teorema}

La comprobación del teorema es:

\begin{sesion}
ghci> quickCheck prop_grado
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop_matrizGrado}}
\begin{code}
prop_matrizGrado :: Grafo Int -> Bool
prop_matrizGrado g = 
    and [grado g u == sumaf u && sumaf u == sumac u | u <- vertices g]
        where sumaf v = sum ((toLists (ma g)) !! (v-1))
              sumac v = sum ((toLists (transpose (ma g))) !! (v-1))
              ma = matrizAdyacencia
\end{code}

\begin{teorema}
  Sean $G_1,G_2$ dos grafos isomorfos y sean $A_1$ y $A_2$ sus matrices 
  de adyacencia respectivas. Entonces existe una matriz de paso $P$, 
  que es regular y verifica:
  $$A_1 = P^t A_2 P$$
\end{teorema}

\begin{teorema}
  Sea $G=(V,A)$ un grafo bipartito de conjuntos de vértices disjuntos 
  $V_1 = \{v_1,\dots,v_k\}$ y $V_2=\{v_{k+1},\dots,v_n\}$, tal que 
  $V=V_1\cup V_2$ y sólo existen aristas que conectan vértices de $V_1$ 
  con vértices de $V_2$. Entonces, con este etiquetado de $V$, la matriz
  de adyacencia de $G$ tiene la forma:
$$
A=\left (\begin{array}{c|c}
\theta  &B      \\ \hline
B^t     &\theta
\end{array}\right)
$$
\end{teorema}

La comprobación del teorema es:

\begin{sesion}
ghci> quickCheck prop_matrizBipartito
+++ OK, passed 100 tests.
\end{sesion}

\begin{code}
prop_matrizBipartito :: Grafo Int -> Property
prop_matrizBipartito g =
    esBipartito g ==>
    all (==0) [ getElem u v m | [u,v] <- (f (fromJust p))]
        where f (xs,ys) = filter p (subsequences xs ++ subsequences ys)
                    where p zs = length zs == 2
              p = conjuntosVerticesDisjuntos g
              m = matrizAdyacencia g
\end{code}
