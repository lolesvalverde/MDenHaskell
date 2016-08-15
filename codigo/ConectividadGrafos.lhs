\ignora{
\begin{code}
module ConectividadGrafos ( esCamino
                          , aristasCamino
                          , verticesCamino
                          , esRecorrido
                          , esCaminoSimple
                          , longitudCamino
                          , todosCaminosBP 
                          , todosCaminosBA 
                          , prop_todosCaminosBA 
                          , estanConectados
                          , distancia
                          , esGeodesica
                          , esCerrado
                          , esCircuito
                          , esCiclo
                          , todosCiclos  
                          , estarConectadosCamino
                          , prop_conectadosRelEqui
                          , componentesConexas
                          , esConexo
                          , prop_caracterizaGrafoConexo
                          , diametro
                          , excentricidad
                          , radio
                          , centro
                          , grosor  
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
import RelacionesHomogeneas     ( esRelacionEquivalencia
                                , clasesEquivalencia
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
                                , grafoEstrella
                                , grafoHeawood
                                , grafoMcGee
                                , grafoTutteCoxeter
                                , grafoPetersen
                                )
import DefinicionesYPropiedades ( orden
                                , tamaño
                                , lazos
                                , eliminaLazos
                                , esCompleto
                                )
\end{code}
}

Una de las aplicaciones de la teoría de grafos es la determinación de trayectos
o recorridos en una red de transporte o de distribución de productos. Así, si
cada vértice representa un punto de interés y cada arista representa una
conexión entre dos puntos, usando grafos como modelos, podemos simplificar el
problema de encontrar la ruta más ventajosa en cada caso.

\subsection{Caminos}

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
  esCamino g c && 
  aristasCamino c == nub (aristasCamino c)
\end{code}

\comentario{Añadir a la sección de conjuntos la función 
  \texttt{(sinRepetidos xs)} que se verifque si \texttt{xs} no tiene elementos
  repetidos y usarla para definir \texttt{esRecorrido}.}

\begin{definicion}
  Un camino que no repite vértices (y, por tanto, tampoco aristas)
  se llama \textbf{camino simple}.
\end{definicion}

La función \texttt{(esCaminoSimple g c)} se verifica si el camino \texttt{c} es
un camino simple. 

\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> esCaminoSimple (grafoCiclo 4) [2,1,4]
-- True
-- >>> esCaminoSimple (grafoCiclo 4) [1,4,3,2,1]
-- True
-- >>> esCaminoSimple (grafoCiclo 4) [4,3,2,1,2]
-- False
esCaminoSimple :: Ord a => Grafo a -> [a] -> Bool
esCaminoSimple g (v:vs) =
  esRecorrido g (v:vs) && 
  verticesCamino vs == vs
\end{code}

\comentario{Corregir la definición de esCaminoSimple para evitar
  contraejemplos como el siguiente
  (esCaminoSimple (creaGrafo [1,2] [(1,1),(1,2)]) [1,1,2] == True).
}

\comentario{Simplificar la definición de \texttt{esCaminoSimple} usando
  \texttt{sinRepetidos}.}

\begin{definicion}
  Se llama \textbf{longitud} de un camino al número de veces que
  se atraviesa una arista en dicho camino.
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

La función \texttt{(todosCaminosBP g u v)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{u} y \texttt{v}, utilizando una algoritmo de búsqueda
en profundidad sobre el grafo \texttt{g}. Este algoritmo recorre el grafo   
de izquierda a derecha y de forma al visitar un nodo, explora todos los   
caminos que pueden continuar por él antes de pasar al siquiente. 

\index{\texttt{todosCaminos}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> todosCaminosBP (grafoCiclo 4) 1 4
-- [[1,4],[1,2,3,4]]
-- >>> todosCaminosBP (grafoCiclo 4) 4 1
-- [[4,3,2,1],[4,1]]
-- >>> todosCaminosBP (creaGrafo [1..4] [(1,2),(3,4)]) 1 4
-- []
-- >>> todosCaminosBP (creaGrafo [1,2] [(1,1),(1,2)]) 1 1
-- [[1]]
todosCaminosBP :: Ord a => Grafo a -> a -> a -> [[a]]
todosCaminosBP g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux ([v:z:zs | v <- adyacentes g' z \\ zs] ++ zss)
        g' = eliminaLazos g
        eliminaLazos h = creaGrafo (vertices h)
                                   [(x,y) | (x,y) <- aristas h, x /= y]
\end{code}
    
La función \texttt{(todosCaminosBA g u v)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{u} y \texttt{v}, utilizando una algoritmo de búsqueda
en anchura sobre el grafo \texttt{g}. Este algoritmo recorre el grafo   
por niveles, de forma que el primer camino de la lista es de longitud mínima. 

\index{\texttt{todosCaminos}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 4
-- G [1,2,3,4] [(1,2),(1,4),(2,3),(3,4)]
-- >>> todosCaminosBA (grafoCiclo 4) 1 4
-- [[1,4],[1,2,3,4]]
-- >>> todosCaminosBA (grafoCiclo 4) 4 1
-- [[4,1],[4,3,2,1]]
-- >>> todosCaminosBA (creaGrafo [1..4] [(1,2),(3,4)]) 1 4
-- []
-- >>> todosCaminosBA (creaGrafo [1,2] [(1,1),(1,2)]) 1 1
-- [[1]]
todosCaminosBA :: Ord a => Grafo a -> a -> a -> [[a]]
todosCaminosBA g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g' z \\ zs])
        g' = eliminaLazos g

\end{code}
    
Vamos a comprobar con QuickCheck que el primer elemento de la
lista que devuelve la función \texttt{(todosCaminosBA g u v)} es de longitud
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

\index{\texttt{prop\_todosCaminosBA}}    
\begin{code}
prop_todosCaminosBA :: Grafo Int -> Property
prop_todosCaminosBA g =
  not (esGrafoNulo g) ==>
  forAll (parDeVertices g)
         (\(x,y) -> let zss = todosCaminosBA g x y
                    in null zss || longitudCamino (head zss) ==
                                   minimum (map longitudCamino zss))
\end{code}

La comprobación es

\begin{sesion}
ghci> quickCheck prop_todosCaminosBA
+++ OK, passed 100 tests:
\end{sesion}

\comentario{Comprobar que con todosCaminosBP no se cumple la propiedad.}
        
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
  | otherwise     = not (null (todosCaminosBA g u v))
\end{code}

\begin{nota}
  La función \texttt{(estanConectados g u v)} no necesita calcular todos los
  caminos entre \texttt{u} y \texttt{v}. Puesto que Haskell utiliza por defecto
  evaluación perezosa, si existe algún camino entre los dos vértices, basta
  calcular el primero para saber que la lista de todos los caminos no es vacía
\end{nota}

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
      Just (longitudCamino (head (todosCaminosBA g u v)))
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
  esRecorrido g c && 
  longitudCamino c == fromJust (distancia g u v)
  where u = head c
        v = last c
\end{code}

\comentario{Comparar la definición de esGeodesica con la siguiente: ¿son
  equivalentes?, ¿cuál es más cercana a la definición matemática?, ¿cuál es más
  corta?}

\begin{code}
esGeodesica2 :: Ord a => Grafo a -> [a] -> Bool
esGeodesica2 g c =
  esCamino g c && 
  longitudCamino c == fromJust (distancia g (head c) (last c))
\end{code}
  
\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos son
  iguales.
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
esCerrado g (v:c) =
  esCamino g c && v == last c
\end{code}

\comentario{En la definición de esCerrado ¿qué pasa con el camino vacío?)}

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
todosCiclos :: Ord a => Grafo a -> a -> [[a]]
todosCiclos g x = [[u] | (u,v) <- lazos g] ++ aux [[x]]
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

\comentario{Corregir la definición de todosCiclos para evitar
  contraejemplos como el siguiente:
  (todosCiclos (creaGrafo [1,2] [(1,1)]) 2 == [[1]])}

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
esAciclico g = aux (vertices g) where
    aux [] = True
    aux (x:xs)
        | not (null (todosCiclos g x)) = False
        | otherwise = aux (xs \\ nub (concat (todosCiclos g x)))
\end{code}


\comentario{La definición de esAciclico se puede simplificar como sigue:}

\begin{code}
esAciclico2 :: Ord a => Grafo a -> Bool
esAciclico2 g = 
  and [null (todosCiclos g x) | x <- vertices g]
\end{code}


\comentario{Desde el punto de vista de la eficiencia no hay grandes diferencias
  entre esAciclico y esAciclico2 como se observa en el siguiente ejemplo:}

\begin{sesion}
ghci> esAciclico2 (completo 100)
False
(2.18 secs, 4,313,432,952 bytes)
ghci> esAciclico (completo 100)
False
(2.23 secs, 4,311,646,816 bytes)
\end{sesion}

\nota{El algoritmo utilizado en la definición de \texttt{(todosCiclos g)} es el
  de búsqueda en anchura. Este algoritmo recorre el grafo por niveles, de forma
  que el primer camino de la lista es de longitud mínima.}

La propiedad es: 

\index{\texttt{prop\_todosCiclos}}    
\begin{code}
prop_todosCiclos :: Grafo Int -> Bool
prop_todosCiclos g = esAciclico g || all p vs
    where vs = filter (not.null.todosCiclos g) (vertices g)                 
          p v =  longitudCamino (head cs) ==
                 minimum (map longitudCamino cs)
                     where cs = todosCiclos g v
\end{code}

La comprobación es:

\begin{sesion}
ghci> quickCheck prop_todosCiclos
+++ OK, passed 100 tests:
\end{sesion}

\comentario{La propiedad \texttt{prop\_todosCiclos} es consecuencia de
  \texttt{prop\_todosCaminosBA} y se puede eliminar.}

\begin{teorema}
  Dado un grafo $G$, la relación $u∼v$ (estar conectados por un camino) es una
  relación de equivalencia.
\end{teorema}

La función \texttt{(estarConectadosCamino g)} devuelve la relación entre   
los vértices del grafo \texttt{g} de estar conectados por un camino en    
él. 

\begin{code}
-- | Ejemplo
-- >>> estarConectadosCamino (creaGrafo [1..4] [(1,2),(2,4)])
-- [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4)]
estarConectadosCamino :: Ord a => Grafo a -> [(a,a)]
estarConectadosCamino g =
  [(u,v) | u <- vs, v <- vs , estanConectados g u v]
  where vs = vertices g
\end{code}

A continuación, comprobaremos el resultado con QuickCheck.
  
\begin{sesion}
ghci> quickCheck prop_conectadosRelEqui
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ConectadosRelEqui}}   
\begin{code}
prop_conectadosRelEqui :: Grafo Int -> Bool
prop_conectadosRelEqui g =
  esRelacionEquivalencia (vertices g) (estarConectadosCamino g)
\end{code}

\begin{definicion}
  Las clases de equivalencia obtenidas por la relación $∼$, estar conectados
  por un camino en un grafo $G$, inducen subgrafos en $G$, los vértices y todas
  las aristas de los caminos que los conectan, que reciben el nombre de
  \textbf{componentes conexas por caminos} de $G$.
\end{definicion}

La función \texttt{(componentesConexas g)} devuelve las componentes conexas por
caminos del grafo \texttt{g}. 
      
\index{\texttt{componentesConexas}}
\begin{code}
-- | Ejemplos
-- >>> componentesConexas (creaGrafo [1..5] [(1,2),(2,3)])
-- [[1,2,3],[4],[5]]
-- >>> componentesConexas (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
-- [[1,2,3],[4,5]]
-- >>> componentesConexas (creaGrafo [1..3] [(1,2),(2,3)])
-- [[1,2,3]]
componentesConexas :: Ord a => Grafo a -> [[a]]
componentesConexas g =
  clasesEquivalencia (vertices g) (estarConectadosCamino g)
\end{code}

\begin{definicion}
  Dado un grafo, diremos que es \textbf{conexo} si la relación $~$ tiene una
  única clase de equivalencia en él; es decir, si el grafo tiene una única
  componente conexa.
\end{definicion}

La función \texttt{(esConexo g)} se verifica si el grafo \texttt{g}
es conexo. 

\index{\texttt{esConexo}}
\begin{code}
-- Ejemplos
-- >>> esConexo (creaGrafo [1..3] [(1,2),(2,3)])
-- True
-- >>> esConexo (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
-- False
esConexo :: Ord a => Grafo a -> Bool
esConexo g = length (componentesConexas g) == 1
\end{code}

\comentario{Buscar definiciones más eficientes de esConexo.}

\begin{teorema}
  Sea $G$ un grafo, $G = (V,A)$ es conexo si y solamente si $forall  u,v \in V$ 
  existe un camino entre $u$ y $v$.
\end{teorema}

Vamos a comprobar el resultado con QuickCheck.

\begin{sesion}
ghci> quickCheck prop_caracterizaGrafoConexo
+++ OK, passed 100 tests.
\end{sesion}      
    
\begin{code}
prop_caracterizaGrafoConexo :: Grafo Int -> Property
prop_caracterizaGrafoConexo g =
    not (esGrafoNulo g) ==>
    esConexo g == and [estanConectados g u v
                      | u <- vertices g, v <- vertices g]
\end{code}

\begin{definicion}
  Sean $G = (V,A)$ un grafo y $v \in V$. Se define la \textbf{excentricidad} de
  $v$ como el máximo de las distancias entre $v$ y el resto de vértices de
  $G$. La denotaremos por $e(v)$.
\end{definicion}

La función \texttt{(excentricidad g v)} devuelve la excentricidad del vértice
\texttt{v} en el grafo \texttt{g}. 

\index{\texttt{excentricidad}}
\begin{code}
-- | Ejemplos
-- >>> let g = creaGrafo [1..3] [(1,2),(2,3),(3,3)]
-- >>> excentricidad g 1
-- Just 2
-- >>> excentricidad g 2
-- Just 1
-- >>> excentricidad (creaGrafo [1..3] [(1,2),(3,3)]) 1
-- Nothing
excentricidad :: Ord a => Grafo a -> a -> Maybe Int
excentricidad g u
  | esGrafoNulo g             = Nothing
  | Nothing `elem` distancias = Nothing
  | otherwise                 = maximum distancias
  where distancias = [distancia g u v | v <- vertices g \\ [u]]
\end{code}

\begin{definicion}
  Sea $G = (V,A)$ un grafo. Se define el \textbf{diámetro} de $G$ como el
  máximo de las distancias entre los vértices en $V$. Lo denotaremos por
  $d(G)$.
\end{definicion}

La función \texttt{(diametro g)} devuelve el diámetro del grafo \texttt{g}. 

\index{\texttt{diametro}}
\begin{code}
-- | Ejemplos
-- >>> diametro (creaGrafo [1..3] [(1,2),(2,3),(3,3)])
-- Just 2
-- >>> diametro (creaGrafo [1..3] [(1,2),(3,3)])
-- Nothing
diametro :: Ord a => Grafo a -> Maybe Int
diametro g 
  | esGrafoNulo g             = Nothing
  | Nothing `elem` distancias = Nothing
  | otherwise                 = maximum distancias
  where vs         = vertices g
        distancias = [distancia g u v | u <- vs, v <- vs, u <= v] 
\end{code}

\begin{definicion}
  Sean $G = (V,A)$ un grafo y $v \in V$. Se define el \textbf{radio} de $G$
  como el mínimo de las excentricidades de sus vértices. Lo denotaremos por
  $r(G)$.
\end{definicion}

La función \texttt{(radio g)} devuelve el radio del grafo \texttt{g}. 

\index{\texttt{radio}}
\begin{code}
-- | Ejemplos
-- >>> radio (creaGrafo [1..3] [(1,2),(2,3),(3,3)])
-- Just 1
-- >>> radio (creaGrafo [1..3] [(1,2),(3,3)])
-- Nothing
radio :: Ord a => Grafo a -> Maybe Int        
radio g
  | esGrafoNulo g     = Nothing
  | Nothing `elem` ds = Nothing
  | otherwise         = minimum ds
  where ds = [excentricidad g v | v <- vertices g]
\end{code}
    
\begin{definicion}
  Sean $G = (V,A)$ un grafo. Llamamos \textbf{centro} del grafo $G$ al conjunto
  de vértices de excentricidad mínima. A estos vértices se les denomina
  \textbf{vértices centrales}.
\end{definicion}

La función \texttt{(centro g)} devuelve el centro del grafo \texttt{g}. Por
ejemplo,

\index{\texttt{centro}}
\begin{code}
-- | Ejemplos
-- >>> centro (creaGrafo [1..3] [(1,2),(2,3),(3,3)])
-- [2]
-- >>> centro (creaGrafo [1..3] [(1,2),(1,3),(2,3)])
-- [1,2,3]
-- >>> centro (creaGrafo [1..3] [(1,2),(3,3)])
-- [1,2,3]
centro :: Ord a => Grafo a -> [a]
centro g = [v | v <- vertices g, excentricidad g v == r]
  where r = radio g
\end{code}
      
\begin{definicion}
  Sean $G = (V,A)$ un grafo. Se llama \textbf{grosor} o \textbf{cintura}
  del grafo $G$ al mínimo de las longitudes de los ciclos de $G$. Si el  
  grafo no posee ciclos (es decir, es un grafo acíclico), se dice que  
  su cintura es infinita.
\end{definicion}

La función \texttt{(grosor g)} devuelve el grosor del grafo \texttt{g}.

\index{\texttt{grosor}}
\begin{code}
-- | Ejemplos
-- >>> grosor (creaGrafo [1,2,3] [(1,2),(2,3)])
-- Nothing
-- >>> grosor (creaGrafo [1,2,3] [(1,1),(1,2),(2,3),(3,4)])
-- Just 0
-- >>> grosor (grafoCiclo 5)
-- Just 5
-- >>> grosor (completo 5)
-- Just 3
-- >>> grosor grafoPetersen
-- Just 5
-- >>> grosor grafoHeawood
-- Just 6
-- >>> grosor grafoMcGee
-- Just 7
-- >>> grosor grafoTutteCoxeter
-- Just 8
grosor :: Ord a => Grafo a -> Maybe Int
grosor g
  | esAciclico g = Nothing
  | esCompleto g = Just 3
  | otherwise    = Just (minimum [longitudCamino (head yss)
                                 | x <- vertices g
                                 , let yss = todosCiclos g x
                                 , not (null yss)])
\end{code}

\comentario{Comprobar la definición de grosor con las familas de grafos.}

\ignora{
  La validación es

  > doctest ConectividadGrafos.lhs 
  Examples: 209  Tried: 209  Errors: 0  Failures: 0
}
