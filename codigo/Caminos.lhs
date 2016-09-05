\ignora{
\begin{code}
module Caminos                  ( esCamino
                                , aristasCamino
                                , verticesCamino
                                , todosCaminos
                                , numeroCaminosDeLongitud
                                , esRecorrido
                                , esCaminoSimple
                                , longitudCamino
                                , todosArcosBP 
                                , todosArcosBA 
                                , prop_todosArcosBA
                                , numeroArcosDeLongitud
                                , estanConectados
                                , distancia
                                , esGeodesica
                                , esCerrado
                                , triangulos
                                , esCircuito
                                , esCiclo
                                , todosCiclos
                                , esAciclico
                                ) where
  
import Data.List                ( (\\)
                                , nub
                                )
import Data.Maybe               ( fromJust)
import Test.QuickCheck          ( Gen
                                , Property
                                , (==>)
                                , elements
                                , forAll
                                , quickCheck
                                )
import Conjuntos                ( variacionesR
                                , sinRepetidos
                                )
import GrafoConListaDeAristas   ( Grafo
                                , adyacentes
                                , aristas
                                , aristaEn
                                , creaGrafo
                                , vertices
                                )
import EjemplosGrafos           ( esGrafoNulo
                                , grafoCiclo
                                , completo
                                , bipartitoCompleto
                                , grafoEstrella
                                , grafoPetersen
                                )
import DefinicionesYPropiedades ( eliminaLazos
                                )
\end{code}
}

Una de las aplicaciones de la teoría de grafos es la determinación de trayectos
o recorridos en una red de transporte o de distribución de productos. Así, si
cada vértice representa un punto de interés y cada arista representa una
conexión entre dos puntos, usando grafos como modelos, podemos simplificar el
problema de encontrar la ruta más ventajosa en cada caso.

\subsection{Definición de camino}

\begin{definicion}
  Sea $G = (V,A)$ un grafo simple y sean $u, v \in V$ dos vértices. Un 
  \textbf{camino} entre $u$ y $v$ es una sucesión de vértices de $G$: 
  $u = v_0, v_1, v_2, \dots, v_{k−1}, v_k = v$ donde
  $\forall i \in \{0,\dots,k-1\}, (v_i,v_{i+1}) \in A$.
\end{definicion}

La función \texttt{(esCamino g c)} se verifica si la sucesión de vértices
\texttt{c} es un camino en el grafo \texttt{g}. 

\index{\texttt{esCamino}}
\begin{code}
-- | Ejemplo
-- >>> grafoCiclo 5
-- G [1,2,3,4,5] [(1,2),(1,5),(2,3),(3,4),(4,5)]
-- >>> esCamino (grafoCiclo 5) [1,2,3,4,5,1]
-- True
-- >>> esCamino (grafoCiclo 5) [1,2,4,5,3,1]
-- False
esCamino :: Ord a => Grafo a -> [a] -> Bool
esCamino g c = all (`aristaEn` g) (zip c (tail c))
\end{code}

La función \texttt{(aristasCamino c)} devuelve la lista de las aristas
recorridas en el camino \texttt{c}.

\index{\texttt{aristasCamino}}
\begin{code}
-- | Ejemplos
-- >>> aristasCamino [1,2,3]
-- [(1,2),(2,3)]
-- >>> aristasCamino [1,2,3,1]
-- [(1,2),(2,3),(1,3)]
-- >>> aristasCamino [1,2,3,2]
-- [(1,2),(2,3),(2,3)]
aristasCamino :: Ord a => [a] -> [(a,a)]
aristasCamino c =
  map parOrdenado (zip c (tail c))
  where parOrdenado (u,v) | u <= v    = (u,v)
                          | otherwise = (v,u)
\end{code}

La función \texttt{(verticesCamino c)} devuelve la lista de las 
vertices recorridas en el camino \texttt{c}.

\index{\texttt{verticesCamino}}
\begin{code}
-- | Ejemplo
-- >>> verticesCamino [1,2,3,1]
-- [1,2,3]
verticesCamino :: Ord a => [a] -> [a]
verticesCamino c = nub c
\end{code}


\begin{definicion}
  Se llama \textbf{longitud} de un camino al número de veces que se atraviesa
  una arista en dicho camino.
\end{definicion}

La función \texttt{(longitudCamino c)} devuelve la longitud del camino
\texttt{c}.

\index{\texttt{longitudCamino}}
\begin{code}
-- | Ejemplo
-- >>> longitudCamino [4,2,7]
-- 2
longitudCamino :: [a] -> Int
longitudCamino c = length c - 1
\end{code}

La función \texttt{(todosCaminos g u v k)} devuelve todos los caminos entre los
vértices \texttt{u} y \texttt{v} en el grafo \texttt{g} que tienen longitud
\texttt{k}.

\index{\texttt{todosCaminos}}
\begin{code}
-- | Ejemplo
-- >>> todosCaminos (creaGrafo [1,2] [(1,2)]) 1 2 3
-- [[1,2,1,2]]
-- >>> todosCaminos (bipartitoCompleto 2 3) 1 3 3
-- [[1,3,1,3],[1,3,2,3],[1,4,1,3],[1,4,2,3],[1,5,1,3],[1,5,2,3]]
todosCaminos :: Ord a => Grafo a -> a -> a -> Int -> [[a]]
todosCaminos g u v 0 = if u == v && elem u (vertices g)
                       then [[u]]
                       else []
todosCaminos g u v 1 = if aristaEn (u,v) g
                       then [[u,v]]
                       else []
todosCaminos g u v k =
    filter (esCamino g) (map f (variacionesR (k-1) vs))
    where vs = vertices g
          f xs = u:xs ++ [v]
\end{code}

\comentario{La definición de \texttt{todosCaminos} se puede simplificar.}

\comentario{¿Se usará la función \texttt{todosCaminos} fuera de este módulo?}

La función \texttt{(numeroCaminosDeLongitud g u v k)} es el número de caminos
de longitud \texttt{k} uniendo los vértices \texttt{u} y \texttt{v} en el grafo
\texttt{g}.

\index{\texttt{numeroCaminosDeLongitud}}
\begin{code}
-- | Ejemplos
-- >>> numeroCaminosDeLongitud (completo 7) 1 4 5
-- 1111
-- >>> numeroCaminosDeLongitud grafoPetersen 1 4 3
-- 5
numeroCaminosDeLongitud :: Ord a => Grafo a -> a -> a -> Int -> Int
numeroCaminosDeLongitud g u v = length . todosCaminos g u v
\end{code}

\comentario{La definición de \texttt{numeroCaminosDeLongitud} se puede
  mejorar.} 

\comentario{¿Se usará la función \texttt{numeroCaminosDeLongitud} fuera de este
  módulo?} 

\subsection{Recorridos}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sean $u,v \in V$. Un camino entre $u$ y $v$ que no
  repite aristas (quizás vértices) se llama \textbf{recorrido}.
\end{definicion}
                                
La función \texttt{(esRecorrido g c)} se verifica si el camino \texttt{c} es un
recorrido en el grafo \texttt{g}. 
       
\begin{code}
-- | Ejemplo
-- >>> esRecorrido (grafoCiclo 4) [2,1,4]
-- True
-- >>> esRecorrido (grafoCiclo 4) [2,1,4,1]
-- False
-- >>> esRecorrido (grafoCiclo 4) [2,1,3]
-- False
esRecorrido :: Ord a => Grafo a -> [a] -> Bool      
esRecorrido g c =
  esCamino g c && sinRepetidos (aristasCamino c)
\end{code}

\subsection{Caminos simples}

\begin{definicion}
  Un camino que no repite vértices (y, por tanto, tampoco aristas)
  se llama \textbf{camino simple}.
\end{definicion}

La función \texttt{(esCaminoSimple g c)} se verifica si el camino \texttt{c} es
un camino simple en el grafo \texttt{g}. 

\begin{code}
-- | Ejemplos
-- >>> esCaminoSimple (creaGrafo [1,2] [(1,1),(1,2)]) [1,1,2]
-- False
-- >>> esCaminoSimple (grafoCiclo 4) [2,1,4]
-- True
-- >>> esCaminoSimple (grafoCiclo 4) [1,4,3,2,1]
-- True
-- >>> esCaminoSimple (grafoCiclo 4) [4,3,2,1,2]
-- False
esCaminoSimple :: Ord a => Grafo a -> [a] -> Bool
esCaminoSimple g [] = True
esCaminoSimple g vs =
  esRecorrido g vs && noRepiteVertices vs
  where noRepiteVertices (x:xs)
            | sinRepetidos (x:xs)              =  True
            | x == last xs && sinRepetidos xs  =  True
            | otherwise                        =  False
\end{code}

La función \texttt{(todosArcosBP g u v)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{u} y \texttt{v}, utilizando una algoritmo de búsqueda
en profundidad sobre el grafo \texttt{g}. Este algoritmo recorre el grafo   
de izquierda a derecha y de forma al visitar un nodo, explora todos los   
caminos que pueden continuar por él antes de pasar al siquiente. 

\index{\texttt{todosArcos}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> todosArcosBP (grafoCiclo 4) 1 4
-- [[1,4],[1,2,3,4]]
-- >>> todosArcosBP (grafoCiclo 4) 4 1
-- [[4,3,2,1],[4,1]]
-- >>> todosArcosBP (creaGrafo [1..4] [(1,2),(3,4)]) 1 4
-- []
-- >>> todosArcosBP (creaGrafo [1,2] [(1,1),(1,2)]) 1 1
-- [[1]]
todosArcosBP :: Ord a => Grafo a -> a -> a -> [[a]]
todosArcosBP g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux ([v:z:zs | v <- adyacentes g' z \\ zs] ++ zss)
        g' = eliminaLazos g
        eliminaLazos h = creaGrafo (vertices h)
                                   [(x,y) | (x,y) <- aristas h, x /= y]
\end{code}
    
La función \texttt{(todosArcosBA g u v)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{u} y \texttt{v}, utilizando una algoritmo de búsqueda
en anchura sobre el grafo \texttt{g}. Este algoritmo recorre el grafo   
por niveles, de forma que el primer camino de la lista es de longitud mínima. 

\index{\texttt{todosArcosBA}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> todosArcosBA (grafoCiclo 4) 1 4
-- [[1,4],[1,2,3,4]]
-- >>> todosArcosBA (grafoCiclo 4) 4 1
-- [[4,1],[4,3,2,1]]
-- >>> todosArcosBA (creaGrafo [1..4] [(1,2),(3,4)]) 1 4
-- []
-- >>> todosArcosBA (creaGrafo [1,2] [(1,1),(1,2)]) 1 1
-- [[1]]
todosArcosBA :: Ord a => Grafo a -> a -> a -> [[a]]
todosArcosBA g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g' z \\ zs])
        g' = eliminaLazos g

\end{code}
    
Vamos a comprobar con QuickCheck que el primer elemento de la
lista que devuelve la función \texttt{(todosArcosBA g u v)} es de longitud
mínima. Para ello, vamos a utilizar la función \texttt{(parDeVertices g)}
que es un generador de pares de vértices del grafo no nulo \texttt{g}. 
Por ejemplo, 

\begin{sesion}
ghci> sample (parDeVertices (creaGrafo [1..9] []))
(3,9)
(9,3)
(7,4)
(4,3)
(2,8)
(7,2)
(8,4)
(5,3)
(7,2)
(3,1)
(7,2)
\end{sesion}

\index{texttt{parDeVertices}}
\begin{code}
parDeVertices :: Grafo Int -> Gen (Int,Int)
parDeVertices g = do
  x <- elements vs
  y <- elements vs
  return (x,y)
  where vs = vertices g
\end{code}

La propiedad es

\index{\texttt{prop\_todosArcosBA}}    
\begin{code}
prop_todosArcosBA :: Grafo Int -> Property
prop_todosArcosBA g =
  not (esGrafoNulo g) ==>
  forAll (parDeVertices g)
         (\(x,y) -> let zss = todosArcosBA g x y
                    in null zss || longitudCamino (head zss) ==
                                   minimum (map longitudCamino zss))
\end{code}

La comprobación es

\begin{sesion}
ghci> quickCheck prop_todosArcosBA
+++ OK, passed 100 tests:
\end{sesion}

Veamos que la función \texttt{(todosArcosBP g u v)} no verifica la       
propiedad.

\begin{sesion}
ghci> quickCheck prop_todosArcosBP
*** Failed! Falsifiable (after 6 tests): 
G [1,2,3,4,5] [(1,2),(1,4),(1,5),(2,2),(2,3),(2,4),(4,4),(4,5),(5,5)]
(5,2)
\end{sesion}

\begin{code}
prop_todosArcosBP :: Grafo Int -> Property
prop_todosArcosBP g =
  not (esGrafoNulo g) ==>
  forAll (parDeVertices g)
         (\(x,y) -> let zss = todosArcosBP g x y
                    in null zss || longitudCamino (head zss) ==
                                   minimum (map longitudCamino zss))
\end{code}

La función \texttt{(numeroArcosDeLongitud g u v k)} devuelve el número de
caminos entre los vértices \texttt{u} y \texttt{v} en el grafo \texttt{g} que
tienen longitud \texttt{k}.

\index{\texttt{numeroArcosDeLongitud}}
\begin{code}
-- | Ejemplos
-- >>> numeroArcosDeLongitud (completo 6) 1 3 4
-- 24
-- >>> numeroArcosDeLongitud grafoPetersen 1 3 4
-- 4
numeroArcosDeLongitud :: Ord a => Grafo a -> a -> a -> Int -> Int
numeroArcosDeLongitud g u v k =
    length (filter p (todosArcosBA g u v))
    where p vs = longitudCamino vs == k
\end{code}

\subsection{Conexión}

\begin{definicion}
  Dado un grafo $G = (V,A)$, sean $u,v \in V$. Si existe algún camino entre $u$
  y $v$ en el grafo $G$ diremos que están \textbf{conectados} y lo denotamos
  por $u ~ v$.
\end{definicion}

La función \texttt{(estanConectados g u v)} se verifica si los vértices
\texttt{u} y \texttt{v} están conectados en el grafo \texttt{g}.

\index{\texttt{estanConectados}}
\begin{code}
-- | Ejemplos
-- >>> estanConectados (creaGrafo [1..4] [(1,2),(2,4)]) 1 4
-- True
-- >>> estanConectados (creaGrafo [1..4] [(1,2),(3,4)]) 1 4
-- False
estanConectados :: Ord a => Grafo a -> a -> a -> Bool
estanConectados g u v
  | esGrafoNulo g = False
  | otherwise     = not (null (todosArcosBA g u v))
\end{code}

\begin{nota}
  La función \texttt{(estanConectados g u v)} no necesita calcular todos los
  caminos entre \texttt{u} y \texttt{v}. Puesto que Haskell utiliza por defecto
  evaluación perezosa, si existe algún camino entre los dos vértices, basta
  calcular el primero para saber que la lista de todos los caminos no es vacía
\end{nota}

\subsection{Distancia}

\begin{definicion}
  Se define la \textbf{distancia} entre $u$ y $v$ en el grafo $G$ como la
  longitud del camino más corto que los une. Si $u$ y $v$ no están conectados,
  decimos que la distancia es infinita.
\end{definicion}

La función \texttt{(distancia g u v)} devuelve la distancia entre los vértices
\texttt{u} y \texttt{v} en el grafo \texttt{g}. En caso de que los vértices no
estén conectados devuelve el valor \texttt{Nothing}. 

\index{\texttt{distancia}}
\begin{code}
-- | Ejemplos
-- >>> distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 1
-- Just 0
-- >>> distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 2
-- Just 1
-- >>> distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 3
-- Just 2
-- >>> distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
-- Nothing
distancia :: Ord a => Grafo a -> a -> a -> Maybe Int    
distancia g u v
  | estanConectados g u v =
      Just (longitudCamino (head (todosArcosBA g u v)))
  | otherwise = Nothing
\end{code}

\begin{definicion}
  Dado $G = (V,A)$ un grafo, sean $u, v \in V$. Un camino entre $u$ y $v$ cuya
  longitud coincide con la distancia entre los vértices se llama
  \textbf{geodésica} entre $u$ y $v$.
\end{definicion}

La función \texttt{(esGeodesica g c)} se verifica si el camino \texttt{c} es
una geodésica entre sus extremos en el grafo \texttt{g}.

\index{\texttt{esGeodesica}}    
\begin{code}
-- | Ejemplos
-- >>> let g = creaGrafo [1..4] [(1,2),(1,3),(2,3),(3,4)]
-- >>> esGeodesica g [1,3,4]
-- True
-- >>> esGeodesica g [1,2,3,4]
-- False
esGeodesica :: Ord a => Grafo a -> [a] -> Bool
esGeodesica g c =
  esCamino g c && 
  longitudCamino c == fromJust (distancia g u v)
  where u = head c
        v = last c
\end{code}

\begin{nota}
  No hace falta imponer en la definición de la función que el camino sea  
  un recorrido, pues el camino de mínima longitud entre dos vértices,   
  es siempre un recorrido. 
\end{nota}

\subsection{Caminos cerrados}

\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos son
  iguales. Diremos que un camino cerrado de longitud tres es un 
  \textbf{triángulo}.
\end{definicion}

La función \texttt{(esCerrado g c)} se verifica si el camino \texttt{c} es
cerrado en el grafo \texttt{g}. 

\index{\texttt{esCerrado}}
\begin{code}
-- | Ejemplos
-- >>> let g = creaGrafo [1..4] [(1,2),(1,3),(2,3),(3,4)]
-- >>> esCerrado g [1,2,3,1]
-- True
-- >>> esCerrado g [1,2,3]
-- False
-- >>> esCerrado g [1,2,4,1]
-- False
esCerrado :: (Ord a) => Grafo a -> [a] -> Bool
esCerrado _ []    = False
esCerrado g (v:c) =
  esCamino g c && v == last c
\end{code}

La función \texttt{(triangulos g v)} devuelve la lista de todos los triángulos
que pasan por el vértice \texttt{v} en el grafo \texttt{g}.

\index{\texttt{triangulos}}
\begin{code}
-- | Ejemplos
-- >>> triangulos (completo 5) 3
-- [[3,1,2,3],[3,1,4,3],[3,2,1,3],[3,2,4,3],[3,4,1,3],[3,4,2,3]]
-- >>> triangulos (grafoCiclo 6) 1
-- []
triangulos :: Ord a => Grafo a -> a -> [[a]]
triangulos g u =
  map f [ [v,w] | v <- us, w <- us, aristaEn (v,w) g, v <= w] 
  where f c = u:c ++ [u]
        us = adyacentes g u 
\end{code}

\comentario{La definición de \texttt{triangulos} es incorrecta. En el primer
  ejemplo faltan triágulos como el [3,2,1,3].}

\subsection{Circuitos}

\begin{definicion}
  Un recorrido en un grafo $G$ se dice \textbf{circuito} si sus extremos son
  iguales.
\end{definicion}

La función \texttt{(esCircuito g c)} se verifica si la sucesión de vértices
\texttt{c} es un circuito en el grafo \texttt{g}.

\index{\texttt{esCircuito}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> esCircuito (grafoCiclo 4) [1,2,3,4,1]
-- True
-- >>> esCircuito (grafoCiclo 4) [1,2,3,4]
-- False
-- >>> esCircuito (grafoCiclo 4) [1,2,3,4,1,4,1]
-- False
esCircuito :: (Ord a) => Grafo a -> [a] -> Bool
esCircuito g c =
  esRecorrido g c && esCerrado g c
\end{code}

\subsection{Ciclos}

\begin{definicion}
  Un camino simple en un grafo $G$ se dice que es un \textbf{ciclo} si 
  sus extremos son iguales.
\end{definicion}

La función \texttt{(esCiclo g c)} se verifica si el camino \texttt{c} es un
ciclo en el grafo \texttt{g}. 

\index{\texttt{esCircuito}}
\begin{code}
-- | Ejemplos
-- >>> esCiclo (grafoCiclo 4) [1,2,1]
-- False
-- >>> esCiclo (grafoCiclo 4) [1,2,3,4,1]
-- True
-- >>> esCiclo (grafoCiclo 4) [1,2,3,4]
-- False
-- >>> esCiclo (grafoCiclo 4) [1,2,3,4,1,4,1]
-- False
esCiclo :: (Ord a) => Grafo a -> [a] -> Bool
esCiclo g c =
  esCaminoSimple g c && esCerrado g c
\end{code}

La función \texttt{(todosCiclos g v)} devuelve todos los ciclos en el grafo
\texttt{g} que pasan por el vértice \texttt{v}.

\index{todosCiclos}
\begin{code}
-- | Ejemplos
-- >>> todosCiclos (grafoCiclo 4) 3
-- [[3,4,1,2,3],[3,2,1,4,3]]
-- >>> todosCiclos (completo 3) 2
-- [[2,3,1,2],[2,1,3,2]]
-- >>> todosCiclos (creaGrafo [1,2,3] [(1,1),(1,2),(1,3),(2,3)]) 1
-- [[1],[1,3,2,1],[1,2,3,1]]
-- >>> todosCiclos (creaGrafo [1,2] [(1,2),(2,2),(2,3)]) 2
-- [[2]]
todosCiclos :: Ord a => Grafo a -> a -> [[a]]
todosCiclos g x = if aristaEn (x,x) g
                  then [x] : aux [[x]]
                  else aux [[x]]
  where aux []       = []
        aux [[z]]    = aux [v:[z] | v <- adyacentes g' z \\ [x]]
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g' z \\
                                                   (head zs:(zs \\ [x]))])
        g' = eliminaLazos g
        eliminaLazos h = creaGrafo (vertices h)
                                   [(x,y) | (x,y) <- aristas h, x /= y]
\end{code}

\comentario{La definición de \texttt{todosCiclos} se puede simplificar.}

\nota{El algoritmo utilizado en la definición de \texttt{(todosCiclos g)} es el
  de búsqueda en anchura. Este algoritmo recorre el grafo por niveles, de forma
  que el primer camino de la lista es de longitud mínima.}

\subsection{Grafos acíclicos}

\begin{definicion}
  Diremos que un grafo $G = (V,A)$ es \textbf{acíclico} si no contiene ningún
  ciclo; es decir, si $\forall v \in V$ no existe ningún camino simple que
  comience y termine en $v$.
\end{definicion}

La función \texttt{(esAciclico g)} se verifica si el grafo \texttt{g} es
acíclico.

\begin{code}
-- | Ejemplo
-- >>> esAciclico (creaGrafo [1..4] [(1,2),(2,4)])
-- True
-- >>> esAciclico (grafoCiclo 5)
-- False
-- >>> esAciclico (grafoEstrella 6)
-- True
esAciclico :: Ord a => Grafo a -> Bool
esAciclico g =
  and [null (todosCiclos g x) | x <- vertices g]
\end{code}

\ignora{
 La validación es:

 > doctest Caminos.lhs
 Examples: 181  Tried: 181  Errors: 0  Failures: 0
}
