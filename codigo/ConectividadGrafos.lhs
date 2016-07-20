
\ignora{
\begin{code}
module ConectividadGrafos (esCamino
                          , esCerrado
                          , longitudCamino
                           
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
  Dado un grafo $G=(V,A)$, sean $u,v \in V$. Diremos que $u$ y $v$
  están \textbf{conectados} si existe algun camino entre ellos en 
  el grafo $G$.
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
longitudCamino :: [a] -> Int
longitudCamino vs = (length vs) - 1
\end{code}

\begin{definicion}
  Se dice \textbf{distancia} entre $u$ y $v$ a la longitud del camino más
  corto que une $u$ y $v$.
\end{definicion}

La función \texttt{(distancia g u v)} devuelve la distancia entre los
vértices \texttt{u} y \texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
distancia (grafoCiclo 7) 1 4     ==  3
distancia (grafoRueda 7) 2 5     ==  2
distancia (grafoEstrella 4) 1 6  ==  1
\end{sesion}

\index{\texttt{distancia}}
\begin{code}
distancia :: Ord a => Grafo a -> a -> a -> Int  
distancia g u v =
    minimum (map longitudCamino (todosCaminos g u v))
\end{code}

\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCerrado g vs)} se verifica si la sucesión de 
vértices \texttt{vs} es un camino cerrado en el grafo \texttt{g}.
Por ejemplo,

\begin{sesion}
ghci> esCerrado (grafoCiclo 5) [1,2,3,4,5,1]
True
ghci> esCerrado (grafoCiclo 5) [1,2,4,5,3,1]
False
ghci> esCerrado grafoThomson [1,4,2,5,3,6]
False
ghci> esCerrado grafoThomson [1,4,2,5,3,6,1]
True
\end{sesion}

\index{\texttt{esCerrado}}
\begin{code}
esCerrado :: (Ord a) => Grafo a -> [a] -> Bool
esCerrado g vs =
    esCamino g vs && head vs == last vs
\end{code}

