\ignora{
\begin{code}
module ConectividadGrafos (esCamino
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
                          , prop_conectadosRelEqui
                          , componentesConexas
                          , esConexo
                          , prop_caracterizaGrafoConexo
                          , diametro
                          , excentricidad
                          , radio
                          , centro 
                          ) where
  
import Conjuntos
import Relaciones
import RelacionesHomogeneas
import Funciones
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import DefinicionesYPropiedades
import Morfismos
    
import Test.QuickCheck
import Data.List
import Data.Maybe
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
\texttt{c} es un camino en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
esCamino (grafoCiclo 5) [1,2,3,4,5,1]  ==  True
esCamino (grafoCiclo 5) [1,2,4,5,3,1]  ==  False
esCamino grafoThomson [1,2,3]          ==  False
esCamino grafoThomson [1,4,2,5,3,6]    ==  True
\end{sesion}

\index{\texttt{esCamino}}
\begin{code}
esCamino :: Ord a => Grafo a -> [a] -> Bool
esCamino g c = all (`aristaEn` g) (zip c (tail c))
\end{code}

La función \texttt{(aristasCamino c)} devuelve la lista de las aristas
recorridas en el camino \texttt{c}.

\begin{sesion}
ghci> aristasCamino [1,2,3,4,5,1]
[(1,2),(2,3),(3,4),(4,5),(5,1)]
ghci> aristasCamino [1,2,4,5,3,1]
[(1,2),(2,4),(4,5),(5,3),(3,1)]
ghci> aristasCamino [1,2,3]
[(1,2),(2,3)]
ghci> aristasCamino [1,4,2,5,3,6]
[(1,4),(4,2),(2,5),(5,3),(3,6)]
\end{sesion}

\comentario{Revisar los ejemplos de aristasCamino.}

\index{\texttt{aristasCamino}}
\begin{code}
aristasCamino :: Ord a => [a] -> [(a,a)]
aristasCamino c =
  map parOrdenado (zip c (tail c))
  where parOrdenado (u,v) | u <= v    = (u,v)
                          | otherwise = (v,u)
\end{code}

La función \texttt{(verticesCamino c)} devuelve la lista de las 
vertices recorridas en el camino \texttt{c}.

\begin{sesion}
verticesCamino [1,2,3,4,5,1]  ==  [1,2,3,4,5]
verticesCamino [1,2,4,5,3,1]  ==  [1,2,4,5,3]
verticesCamino [1,2,3]        ==  [1,2,3]
verticesCamino []             ==  []
\end{sesion}

\index{\texttt{verticesCamino}}
\begin{code}
verticesCamino :: Ord a => [a] -> [a]
verticesCamino c = nub c
\end{code}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sean $u,v \in V$. Un camino entre $u$ y $v$ que no
  repite aristas (quizás vértices) se llama \textbf{recorrido}.
\end{definicion}

La función \texttt{(esRecorrido g c)} se verifica si el camino \texttt{c} es un
recorrido en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
esRecorrido (grafoRueda 5) [1,2,3,4,1,2,5]  ==  False
esRecorrido ['a'..'z']       ==  True
esRecorrido [1,2,4,6,4]      ==  True
esRecorrido [1,2,1,3,4]      ==  True       
\end{sesion}
       
\begin{code}
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
un arco. Por ejemplo,

\begin{sesion}
esCaminoSimple [1..4]       ==  True
esCaminoSimple [1,2,6,1]    ==  True
esCaminoSimple [1,2,3,1,4]  ==  False
esCaminoSimple ['a'..'f']   ==  True
\end{sesion}

\comentario{Corregir ejemplos de esCaminoSimple.}

\begin{code}
esCaminoSimple :: Ord a => Grafo a -> [a] -> Bool
esCaminoSimple g (v:vs) =
  esCamino g (v:vs) && 
  verticesCamino vs == vs
\end{code}

\comentario{Simplificar la definición de \texttt{esCaminoSimple} usando
  \texttt{sinRepetidos}.}

\begin{definicion}
  Se llama \textbf{longitud} de un camino al número de veces que
  se atraviesa una arista en dicho camino.
\end{definicion}

La función \texttt{(longitudCamino c)} devuelve la longitud del camino
\texttt{c}. Por ejemplo,

\begin{sesion}
longitudCamino [1,2,3,4]   == 3
longitudCamino ['a'..'z']  == 25
longitudCamino [2,4..10]   == 4            
\end{sesion}

\index{\texttt{longitudCamino}}
\begin{code}
longitudCamino :: [a] -> Int
longitudCamino c = length c - 1
\end{code}

La función \texttt{(todosCaminosBP g u v)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{u} y \texttt{v}, utilizando una algoritmo de búsqueda
en profundidad sobre el grafo \texttt{g}. Este algoritmo recorre el grafo   
de izquierda a derecha y de forma al visitar un nodo, explora todos los   
caminos que pueden continuar por él antes de pasar al siquiente. 

\begin{sesion}
ghci> todosCaminosBP (grafoCiclo 7) 1 6
[[1,2,3,4,5,6],[1,7,6]]
ghci> todosCaminosBP (grafoRueda 7) 2 5
[[2,1,5],[2,3,1,5],[2,3,4,1,5],[2,7,6,1,5],[2,7,1,5],[2,1,4,5],
 [2,3,1,4,5],[2,7,6,1,4,5],[2,7,1,4,5],[2,1,3,4,5],[2,7,6,1,3,4,5],
 [2,7,1,3,4,5],[2,3,4,5],[2,1,6,5],[2,3,1,6,5],[2,3,4,1,6,5],
 [2,7,1,6,5],[2,1,7,6,5],[2,3,1,7,6,5],[2,3,4,1,7,6,5],[2,7,6,5]]
ghci> todosCaminosBP (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
[]
\end{sesion}

\index{\texttt{todosCaminos}}
\begin{code}
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

\begin{sesion}
ghci> todosCaminosBA (grafoCiclo 7) 1 6
[[1,7,6],[1,2,3,4,5,6]]
ghci> todosCaminosBA (grafoRueda 7) 2 5
[[2,1,5],[2,3,1,5],[2,7,1,5],[2,1,4,5],[2,3,4,5],[2,1,6,5],[2,7,6,5],
 [2,3,4,1,5],[2,7,6,1,5],[2,3,1,4,5],[2,7,1,4,5],[2,1,3,4,5],
 [2,3,1,6,5],[2,7,1,6,5],[2,1,7,6,5],[2,7,6,1,4,5],[2,7,1,3,4,5],
 [2,3,4,1,6,5],[2,3,1,7,6,5],[2,7,6,1,3,4,5],[2,3,4,1,7,6,5]]
ghci> todosCaminosBA (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
[]
\end{sesion}

\index{\texttt{todosCaminos}}
\begin{code}
todosCaminosBA :: Ord a => Grafo a -> a -> a -> [[a]]
todosCaminosBA g x y = aux [[y]]
  where aux []       = []
        aux ([]:zss) = aux zss
        aux ((z:zs):zss)
          | z == x    = (z:zs) : aux zss
          | otherwise = aux (zss ++ [v:z:zs | v <- adyacentes g' z \\ zs])
        g' = eliminaLazos g
        eliminaLazos h = creaGrafo (vertices h)
                                   [(x,y) | (x,y) <- aristas h, x /= y]
\end{code}
    
Vamos a comprobar con \texttt{QuickCheck} que el primer elemento de la
lista que devuelve la función \texttt{todosCaminosBA g u v)} es de longitud
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
ghci> quickCheckWith (stdArgs {maxSize=15}) prop_todosCaminosBA
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

\begin{sesion}
ghci> estanConectados (grafoCiclo 7) 1 6
True
ghci> estanConectados (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
False
ghci> estanConectados grafoNulo 1 4
False
\end{sesion}

\index{\texttt{estanConectados}}
\begin{code}
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
estén conectados devuelve el valor \texttt{Nothing}. Por ejemplo,

\begin{sesion}
distancia (grafoCiclo 7) 1 4                    ==  Just 3
distancia (grafoRueda 7) 2 5                    ==  Just 2
distancia (grafoEstrella 4) 1 3                 ==  Just 1
distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 4  ==  Nothing
distancia grafoNulo 2 3                         ==  Nothing
\end{sesion}

\index{\texttt{distancia}}
\begin{code}
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

\begin{sesion}
esGeodesica (grafoCiclo 7) [1,2,3,4,5,6]            == False
esGeodesica (grafoCiclo 7) [1,7,6]                  == True            
esGeodesica (grafoRueda 7) [2,1,5]                  == True
esGeodesica (creaGrafo [1..4] [(1,2),(2,3)]) [1,4]  == False
esGeodesica grafoNulo [1,4]                         == False
\end{sesion}

\index{\texttt{esGeodesica}}    
\begin{code}
esGeodesica :: Ord a => Grafo a -> [a] -> Bool
esGeodesica g c =
  esCamino g c && 
  longitudCamino c == fromJust (distancia g u v)
  where u = head c
        v = last c
\end{code}
  
\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos son
  iguales.
\end{definicion}

La función \texttt{(esCerrado g c)} se verifica si el camino \texttt{c} es
cerrado en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
esCerrado (grafoCiclo 5) [1,2,3,4,5,1]  ==  True
esCerrado (grafoCiclo 5) [1,2,4,5,3,1]  ==  False
esCerrado grafoThomson [1,4,2,5,3,6]    ==  False
esCerrado grafoThomson [1,4,2,5,3,6,1]  ==  True
esCerrado grafoNulo [1,2,1]             ==  False
\end{sesion}

\index{\texttt{esCerrado}}
\begin{code}
esCerrado :: (Ord a) => Grafo a -> [a] -> Bool
esCerrado g c =
  esCamino g c && head c == last c
\end{code}
  
\begin{definicion}
  Un recorrido en un grafo $G$ se dice \textbf{circuito} si sus extremos son
  iguales.
\end{definicion}

La función \texttt{(esCircuito g c)} se verifica si la sucesión de vértices
\texttt{c} es un circuito en el grafo \texttt{g}.  Por ejemplo,

\begin{sesion}
esCircuito (grafoCiclo 5) [1,2,3,4,5,1]    ==  True
esCircuito (grafoCiclo 3) [1,2,3,1,2,4,1]  ==  False
esCircuito grafoThomson [1,4,2,5,3,6]      ==  False
esCircuito grafoThomson [1,4,2,5,3,6,1]    ==  True
esCircuito grafoNulo [1,2,1]               ==  False     
\end{sesion}

\index{\texttt{esCircuito}}
\begin{code}
esCircuito :: (Ord a) => Grafo a -> [a] -> Bool
esCircuito g c =
    esRecorrido g c && esCerrado g c
\end{code}

\begin{definicion}
  Un camino simple en un grafo $G$ se dice que es un \textbf{circuito} si 
  sus extremos son iguales.
\end{definicion}

La función \texttt{(esCiclo g c)} se verifica si el camino \texttt{c} es un
ciclo en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
esCiclo (grafoCiclo 5) [1,2,3,4,5,1]    ==  True
esCiclo (grafoCiclo 3) [1,2,3,1,2,4,1]  ==  False
esCiclo grafoThomson [1,4,2,5,3,6]      ==  False
esCiclo grafoThomson [1,4,2,5,3,6,1]    ==  True
esCiclo grafoNulo [1,2,1]               ==  False
\end{sesion}

\index{\texttt{esCircuito}}
\begin{code}
esCiclo :: (Ord a) => Grafo a -> [a] -> Bool
esCiclo g c =
  esCaminoSimple g c && esCerrado g c
\end{code}
    
\begin{teorema}
  Dado un grafo $G$, la relación $u∼v$ (estar conectados por un camino) es una
  relación de equivalencia.
\end{teorema}

La función \texttt{(estarConectadosCamino g)} devuelve la relación entre   
los vértices del grafo \texttt{g} de estar conectados por un camino en    
él.

\begin{code}
estarConectadosCamino :: Ord a => Grafo a -> [(a,a)]
estarConectadosCamino g =
  [(u,v) | u <- vs, v <- vs , estanConectados g u v]
  where vs = vertices g
\end{code}

A continuación, comprobaremos el resultado con QuickCheck.
  
\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=10}) prop_conectadosRelEqui
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
caminos del grafo \texttt{g}. Por ejemplo,

\begin{sesion}
ghci> componentesConexas grafoPetersen
[[1,2,3,4,5,6,7,8,9,10]]
ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3)])
[[1,2,3],[4],[5]]
ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
[[1,2,3],[4,5]]
ghci> componentesConexas grafoNulo
[]
\end{sesion}
      
\index{\texttt{componentesConexas}}
\begin{code}
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
es conexo. Por ejemplo,

\begin{sesion}
esConexo (completo 5)                      == True
esConexo (creaGrafo [1..5] [(1,2),(2,3)])  == False
esConexo (creaGrafo [1..3] [(1,2),(2,3)])  == True
esConexo grafoNulo                         == False    
\end{sesion}

\index{\texttt{esConexo}}
\begin{code}
esConexo :: Ord a => Grafo a -> Bool
esConexo g = length (componentesConexas g) == 1
\end{code}

\comentario{Comentar la complejidad de esConexo y posibles mejoras de la
  definición.} 

\begin{teorema}
  Sea $G$ un grafo, $G = (V,A)$ es conexo si y solamente si $forall  u,v \in V$ 
  existe un camino entre $u$ y $v$.
\end{teorema}

Vamos a comprobar el resultado con QuickCheck.

\begin{sesion}
λ> quickCheckWith (stdArgs {maxSize=10}) prop_caracterizaGrafoConexo
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
\texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
excentricidad (grafoCiclo 8) 5      ==  Just 4
excentricidad (grafoRueda 7) 4      ==  Just 2
excentricidad (grafoRueda 7) 1      ==  Just 1
excentricidad grafoPetersen  6      ==  Just 2
let g = creaGrafo [1,2,3] [(1,2)]
excentricidad g 3                   ==  Nothing
excentricidad grafoNulo 3           ==  Nothing
\end{sesion}

\index{\texttt{excentricidad}}
\begin{code}
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

La función \texttt{(diametro g)} devuelve el diámetro del grafo \texttt{g}. Por
ejemplo,

\begin{sesion}
diametro (grafoCiclo 8)      ==  Just 4
diametro (grafoRueda 7)      ==  Just 2
diametro grafoPetersen       ==  Just 2
let g = creaGrafo [1,2,3] [(1,2)]
diametro g                   ==  Just 1
diametro grafoNulo           ==  Nothing       
\end{sesion}

\index{\texttt{diametro}}
\begin{code}
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

La función \texttt{(radio g)} devuelve el radio del grafo \texttt{g}. Por
ejemplo,

\begin{sesion}
radio (grafoCiclo 8)      ==  Just 4
radio (grafoRueda 7)      ==  Just 1
radio grafoPetersen       ==  Just 2
let g = creaGrafo [1,2,3] [(1,2)]
radio g                   ==  Just 1
radio grafoNulo           ==  Nothing
\end{sesion}

\index{\texttt{radio}}
\begin{code}
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

\begin{sesion}
centro (grafoEstrella 5)  ==  [1]
centro (grafoCiclo 4)     ==  [1,2,3,4]
centro grafoPetersen      ==  [1,2,3,4,5,6,7,8,9,10]
centro (grafoRueda 5)     ==  [1]
centro grafoNulo          ==  []
\end{sesion}      

\index{\texttt{centro}}
\begin{code}
centro :: Ord a => Grafo a -> [a]
centro g = [v | v <- vertices g, excentricidad g v == r]
  where r = radio g
\end{code}
      
\begin{definicion}
  Sean $G = (V,A)$ un grafo. Se llama \textbf{grosor} o \textbf{cintura}
  del grafo $G$ al máximo de las longitudes de los ciclos de $G$.
\end{definicion}

