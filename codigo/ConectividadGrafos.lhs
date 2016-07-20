
\ignora{
\begin{code}
module ConectividadGrafos (esCamino
                          , todosCaminos
                          , estanConectados
                          , longitudCamino
                          , distancia
                          , esGeodesica
                          , esCerrado                           
                          ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import ConjuntosRelacionesYFunciones
import DefinicionesYPropiedades
import Morfismos
    
import Test.QuickCheck
import Data.List
\end{code}
}

Una de las aplicaciones de la Teoría de Grafos es la determinación
de trayectos o recorridos en una red de transporte o de distribución 
de productos. Así, si cada vértice representa un punto de interés y
cada arista representa una conexión entre dos puntos, usando grafos 
como modelos, podemos simplificar el problema de encontrar la ruta más
ventajosa en cada caso.

\subsection{Caminos}

\begin{definicion}
  Sea $G=(V,A)$ un grafo simple y sean $u,v \in V$ dos vértices. Un 
  \textbf{camino} entre $u$ y $v$ es una sucesión de vértices de $G$: 
  $u=v_0,v_1,v_2,\dots,v_{k−1},v_k=v$ donde $\forall i \in \{0,\dots,k-1\},
  \allowbreak v_i\in V y (v_i,v_{i+1})\in A$.
\end{definicion}

La función \texttt{(esCamino g vs)} se verifica si la sucesión de 
vértices \texttt{vs} es un camino en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
ghci> esCamino (grafoCiclo 5) [1,2,3,4,5,1]
True
ghci> esCamino (grafoCiclo 5) [1,2,4,5,3,1]
False
ghci> esCamino grafoThomson [1,2,3]
False
ghci> esCamino grafoThomson [1,4,2,5,3,6]
True
\end{sesion}

\index{\texttt{esCamino}}
\begin{code}
esCamino :: (Ord a) => Grafo a -> [a] -> Bool
esCamino g vs = all (`elem` (vertices g)) vs &&
                all p (zip vs (tail vs))
      where p (u,v) = elem (u,v) (aristas g)||
                      elem (v,u) (aristas g)
\end{code}

La función \texttt{(todosCaminos g inicio final)} devuelve una   
lista con todos los caminos posibles entre los vértices \texttt{inicio} y 
\texttt{final}, encontrados usando un algoritmo de búsqueda en                
profundidad sobre el grafo \texttt{g}.

\begin{sesion}
ghci> todosCaminos (grafoCiclo 7) 1 6
[[1,2,3,4,5,6],[1,7,6]]
ghci> todosCaminos (grafoRueda 7) 2 5
[[2,1,3,4,5],[2,1,4,5],[2,1,5],[2,3,1,4,5],[2,3,1,5],
 [2,3,4,1,5],[2,3,4,5],[2,7,1,3,4,5],[2,7,1,4,5],[2,7,1,5],
 [2,7,6,1,3,4,5],[2,7,6,1,4,5],[2,7,6,1,5],[2,7,6,5]]
ghci> todosCaminos (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
[]
\end{sesion}

\index{\texttt{todosCaminos}}          
\begin{code}
todosCaminos :: Eq a => Grafo a -> a -> a -> [[a]]
todosCaminos g inicio final = bp [inicio] []            
    where bp [] [] = []
          bp [] vis = if (last vis == final) then [vis] else []      
          bp (v:vs) vis 
              | v == final = [vis ++ [v]]
              | elem v vis = bp vs vis
              | otherwise = bp (adyacentes g v) (vis ++ [v]) ++
                            bp vs vis 
\end{code}

\begin{definicion}
  Dado un grafo $G=(V,A)$, sean $u,v \in V$. Si existe algún
  camino entre $u$ y $v$ en el grafo $G$ diremos que están
  \textbf{conectados} y lo denotamos por $u~v$.
\end{definicion}

La función \texttt{(estanConectados g u v)} se verifica si los 
vértices \texttt{u} y \texttt{v} están conectados en el grafo 
\texttt{g}.

\begin{sesion}
ghci> estanConectados (grafoCiclo 7) 1 6
True
ghci> estanConectados (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
False
\end{sesion}

\index{\texttt{estanConectados}}
\begin{code}
estanConectados ::  Eq a => Grafo a -> a -> a -> Bool
estanConectados g u = not.null.todosCaminos g u
\end{code}

\begin{definicion}
  Se llama \textbf{longitud} de un camino al número de veces que
  se atraviesa una arista en dicho camino.
\end{definicion}

La función \texttt{(longitudCamino vs)} devuelve la longitud del camino
\texttt{vs}. Por ejemplo,

\begin{sesion}
longitudCamino [1,2,3,4]   == 3
longitudCamino ['a'..'z']  == 25
longitudCamino [2,4..10]   == 4            
\end{sesion}

\index{\texttt{longitudCamino}}
\begin{code}
longitudCamino :: Num b => [a] -> b
longitudCamino vs = (genericLength vs) - 1
\end{code}

\begin{definicion}
  Se define la \textbf{distancia} entre $u$ y $v$ en el grafo $G$
  como la longitud del camino más corto que los une. Si $u$ y $v$
  no están conectados, decimos que la distancia es infinita.
\end{definicion}

La función \texttt{(distancia g u v)} devuelve la distancia entre los
vértices \texttt{u} y \texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
distancia (grafoCiclo 7) 1 4                    ==  3.0
distancia (grafoRueda 7) 2 5                    ==  2.0
distancia (grafoEstrella 4) 1 6                 ==  1.0
distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 4  ==  Infinity
\end{sesion}

\index{\texttt{distancia}}
\begin{code}
distancia :: Ord a => Grafo a -> a -> a -> Double     
distancia g u v | estanConectados g u v =
                    minimum (map longitudCamino (todosCaminos g u v))
                | otherwise = 1/0
\end{code}

\begin{definicion}
  Dado $G=(V,A)$ un grafo, sean $u,v \in V$. Un camino entre
  $u$ y $v$ cuya longitud coincide con la distancia entre los
  vértices se llama \textbf{geodésica} entre $u$ y $v$.
\end{definicion}

La función \texttt{(esGeodesica g vs u v)} se verifica si el camino
\texttt{vs} es una geodesica entre \texttt{u} y \texttt{v} en
el grafo \texttt{g}.

\begin{sesion}
esGeodesica (grafoCiclo 7) [1,2,3,4,5,6] 1 6           == False
esGeodesica (grafoCiclo 7) [1,7,6] 1 6                 == True            
esGeodesica (grafoRueda 7) [2,1,5] 2 5                 == True
esGeodesica (grafoRueda 7) [2,1,4,5] 2 5               == False
esGeodesica (creaGrafo [1..4] [(1,2),(2,3)]) [1,4] 1 4 == False
\end{sesion}

\index{\texttt{esGeodesica}}    
\begin{code}
esGeodesica :: Ord a => Grafo a -> [a] -> a -> a -> Bool
esGeodesica g vs u v = esCamino g vs &&
    longitudCamino vs == distancia g u v
\end{code}
  
\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCerrado g vs)} se verifica si la sucesión de 
vértices \texttt{vs} es un camino cerrado en el grafo \texttt{g}.
Por ejemplo,

\begin{sesion}
esCerrado (grafoCiclo 5) [1,2,3,4,5,1]  ==  True
esCerrado (grafoCiclo 5) [1,2,4,5,3,1]  ==  False
esCerrado grafoThomson [1,4,2,5,3,6]    ==  False
esCerrado grafoThomson [1,4,2,5,3,6,1]  ==  True
\end{sesion}

\index{\texttt{esCerrado}}
\begin{code}
esCerrado :: (Ord a) => Grafo a -> [a] -> Bool
esCerrado g vs =
    esCamino g vs && head vs == last vs
\end{code}

\begin{teorema}
  Dado un grafo $G$, la relación $u∼v$ (estar conectados por un camino)
  es una relación de equivalencia.
\end{teorema}

A continuación, comprobaremos el resultado con \texttt{quickCheck}.
  
\begin{sesion}
ghci> quickCheck prop_ConectadosRelEqui
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ConectadosRelEqui}}   
\begin{code}
prop_ConectadosRelEqui :: Grafo Int -> Int -> Int -> Int -> Bool
prop_ConectadosRelEqui g u v w =
    reflexiva && simetrica && transitiva
    where reflexiva = estanConectados g u u
          simetrica =
              not (estanConectados g u v) || estanConectados g v u
          transitiva =
              not(estanConectados g u v && estanConectados g v w)
                 || estanConectados g u w
\end{code}                                    

\begin{definicion}
  Las clases de equivalencia obtenidas por la relación $∼$, 
  estar conectados por un camino en un grafo $G$,  inducen subgrafos 
  en $G$, los vértices y todas las aristas de los caminos que los conectan, 
  que reciben el nombre de \textbf{componentes conexas por caminos} de $G$. 
  Cuando sólo hay una clase de equivalencia; es decir, cuando el grafo solo
  tiene una componente conexa, se dice que el grafo es \textbf{conexo}.
\end{definicion}

La función \texttt{(componentesConexas g)} devuelve las componentes 
conexas por caminos del grafo \texttt{g}.

\begin{sesion}
ghci> componentesConexas grafoPetersen
[[1,2,3,4,5,6,7,8,9,10]]
ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3)])
[[1,2,3],[4],[5]]
ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
[[1,2,3],[4,5]]
\end{sesion}
      
\index{\texttt{componentesConexas}}
\begin{code}
componentesConexas :: Eq a => Grafo a -> [[a]]
componentesConexas g = aux (vertices g)
      where aux []     = []
            aux (v:vs) = c: aux (vs \\ c)
                 where c = filter (estanConectados g v) (v:vs)
\end{code}

\begin{definicion}
  Dado un grafo, diremos que es \textbf{conexo} si la relación $~$
  tiene una única clase de equivalencia en él; es decir, si el grafo
  tiene una única componente conexa.
\end{definicion}

La función \texttt{(esConexo g)} se verifica si el grafo \texttt{g}
es conexo.

\begin{sesion}
esConexo (completo 5)                           == True
esConexo (creaGrafo [1..5] [(1,2),(2,3)])       == False
esConexo (creaGrafo [1..3] [(1,2),(2,3)])       == True
esConexo (creaGrafo [] [])                      == False    
\end{sesion}

\index{\texttt{esConexo}}
\begin{code}
esConexo :: Eq a => Grafo a -> Bool
esConexo g = length (componentesConexas g) == 1
\end{code}             

