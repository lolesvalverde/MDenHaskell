\ignora{
\begin{code}
module ConectividadGrafos (esCamino
                          , aristasCamino
                          , esRecorrido
                          , esArco
                          , todosArcos
                          , estanConectados
                          , longitudCamino
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

Una de las aplicaciones de la teoría de grafos es la determinación de trayectos
o recorridos en una red de transporte o de distribución de productos. Así, si
cada vértice representa un punto de interés y cada arista representa una
conexión entre dos puntos, usando grafos como modelos, podemos simplificar el
problema de encontrar la ruta más ventajosa en cada caso.

\subsection{Caminos}

\begin{definicion}
  Sea $G = (V,A)$ un grafo simple y sean $u,v \in V$ dos vértices. Un 
  \textbf{camino} entre $u$ y $v$ es una sucesión de vértices de $G$: 
  $u = v_0, v_1, v_2, \dots, v_{k−1}, v_k = v$ donde
  $\forall i \in \{0,\dots,k-1\}, (v_i,v_{i+1}) \in A$.
\end{definicion}

La función \texttt{(esCamino g vs)} se verifica si la sucesión de vértices
\texttt{vs} es un camino en el grafo \texttt{g}. Por ejemplo,

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
esCamino :: Ord a => Grafo a -> [a] -> Bool
esCamino g vs = all (`elem` (vertices g)) vs &&
                all p (zip vs (tail vs))
      where p (u,v) = elem (u,v) (aristas g)||
                      elem (v,u) (aristas g)
\end{code}

\comentario{Se puede simplificar esCamino usando aristaEn.}

La función \texttt{(aristasCamino vs)} devuelve la lista de las 
aristas recorridas en el camino \texttt{vs}.

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

\index{\texttt{aristasCamino}}
\begin{code}
aristasCamino :: Eq a => [a] -> [(a,a)]
aristasCamino vs = zip vs (tail vs)
\end{code}
 
\begin{definicion}
  Sea $G=(V,A)$ un grafo y sean $u,v \in V$. Un camino entre
  $u$ y $v$ que no repite aristas (quizás vértices) se llama 
  \textbf{recorrido}.
\end{definicion}

La función \texttt{(esRecorrido g c)} se verifica si el camino
\texttt{c} es un recorrido.

\begin{sesion}
esRecorrido (grafoRueda 5) [1,2,3,4,1,2,5]  ==  False
esRecorrido ['a'..'z']       ==  True
esRecorrido [1,2,4,6,4]      ==  True
esRecorrido [1,2,1,3,4]      ==  True       
\end{sesion}
       
\begin{code}
esRecorrido :: Eq a => [a] -> Bool      
esRecorrido c =
    aristasCamino c == nub (aristasCamino c)
\end{code}      

\begin{definicion}
  Un camino que no repite vértices (y, por tanto, tampoco aristas)
  se llama \textbf{arco}.
\end{definicion}

La función \texttt{(esArco c)} se verifica si el camino
\texttt{vs} es un arco.

\begin{sesion}
esArco [1..4]              == True
esArco ([1..5] ++ [1..4])  ==  False
esArco [1,2,1,3,4]         ==  False
esArco ['a'..'f']          == True
\end{sesion}

\begin{code}
esArco :: Ord a => [a] -> Bool
esArco vs = nub vas == vas
    where vas = map fst (aristasCamino vs)
\end{code}
    
La función \texttt{(todosRecorridos g inicio final)} devuelve una   
lista con todos los caminos posibles entre los vértices \texttt{inicio} y 
\texttt{final}, encontrados usando un algoritmo de búsqueda en                
profundidad sobre el grafo \texttt{g}.

\begin{sesion}
ghci> todosArcos (grafoCiclo 7) 1 6
[[1,2,3,4,5,6],[1,7,6]]
ghci> todosArcos (grafoRueda 7) 2 5
[[2,1,3,4,5],[2,1,4,5],[2,1,5],[2,3,1,4,5],[2,3,1,5],
 [2,3,4,1,5],[2,3,4,5],[2,7,1,3,4,5],[2,7,1,4,5],[2,7,1,5],
 [2,7,6,1,3,4,5],[2,7,6,1,4,5],[2,7,6,1,5],[2,7,6,5]]
ghci> todosArcos (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
[]
\end{sesion}

\index{\texttt{todosArcos}}          
\begin{code}
todosArcos :: Eq a => Grafo a -> a -> a -> [[a]]
todosArcos g inicio final = bp [inicio] []            
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
ghci> estanConectados grafoNulo 1 4
False
\end{sesion}

\index{\texttt{estanConectados}}
\begin{code}
estanConectados ::  Eq a => Grafo a -> a -> a -> Bool
estanConectados g u v | null (vertices g) = False
                      | otherwise = not (null (todosArcos g u v))
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
distancia grafoNulo 2 3                         ==  Infinity
\end{sesion}

\index{\texttt{distancia}}
\begin{code}
distancia :: Ord a => Grafo a -> a -> a -> Double     
distancia g u v | estanConectados g u v =
                    minimum (map longitudCamino (todosArcos g u v))
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
esGeodesica (grafoCiclo 7) [1,2,3,4,5,6] 1 6            == False
esGeodesica (grafoCiclo 7) [1,7,6] 1 6                  == True            
esGeodesica (grafoRueda 7) [2,1,5] 2 5                  == True
esGeodesica (grafoRueda 7) [2,1,4,5] 2 5                == False
esGeodesica (creaGrafo [1..4] [(1,2),(2,3)]) [1,4] 1 4  == False
esGeodesica grafoNulo [1,4] 1 4                         == False
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
esCerrado grafoNulo [1,2,1]             ==  False
\end{sesion}

\index{\texttt{esCerrado}}
\begin{code}
esCerrado :: (Ord a) => Grafo a -> [a] -> Bool
esCerrado g vs =
    esCamino g vs && head vs == last vs
\end{code}
  
\begin{definicion}
  Un recorrido en un grafo $G$ se dice \textbf{circuito} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCircuito g vs)} se verifica si la sucesión de 
vértices \texttt{vs} es un circuito en el grafo \texttt{g}.
Por ejemplo,

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
esCircuito g vs =
    esRecorrido vs && esCerrado g vs
\end{code}

\begin{definicion}
  Un arco en un grafo $G$ se dice \textbf{circuito} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCiclo g vs)} se verifica si la sucesión de 
vértices \texttt{vs} es un ciclo en el grafo \texttt{g}.
Por ejemplo,

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
esCiclo g vs =
    esArco vs && esCerrado g vs
\end{code}
    
\begin{teorema}
  Dado un grafo $G$, la relación $u∼v$ (estar conectados por un camino)
  es una relación de equivalencia.
\end{teorema}

A continuación, comprobaremos el resultado con \texttt{quickCheck}.
  
\begin{sesion}
ghci> quickCheck prop_conectadosRelEqui
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ConectadosRelEqui}}   
\begin{code}
prop_conectadosRelEqui :: Grafo Int -> Int -> Int -> Int -> Bool
prop_conectadosRelEqui g u v w =
    reflexiva && simetrica && transitiva
    where reflexiva = estanConectados g u u
          simetrica =
              not (estanConectados g u v) || estanConectados g v u
          transitiva =
              not (estanConectados g u v && estanConectados g v w)
                 || estanConectados g u w           
\end{code}

\begin{definicion}
  Las clases de equivalencia obtenidas por la relación $∼$, 
  estar conectados por un camino en un grafo $G$,  inducen subgrafos 
  en $G$, los vértices y todas las aristas de los caminos que los conectan, 
  que reciben el nombre de \textbf{componentes conexas por caminos} de $G$.
\end{definicion}

La función \texttt{(componentesConexas g)} devuelve las componentes 
conexas por caminos del grafo \texttt{g}. Por ejemplo,

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
es conexo. Por ejemplo,

\begin{sesion}
esConexo (completo 5)                      == True
esConexo (creaGrafo [1..5] [(1,2),(2,3)])  == False
esConexo (creaGrafo [1..3] [(1,2),(2,3)])  == True
esConexo grafoNulo                         == False    
\end{sesion}

\index{\texttt{esConexo}}
\begin{code}
esConexo :: Eq a => Grafo a -> Bool
esConexo g = length (componentesConexas g) == 1
\end{code}             

\begin{teorema}
  Sea $G$ un grafo, $G=(V,A)$ es conexo si y solamente si $forall  u,v \in V$ 
  existe un camino entre $u$ y $v$.
\end{teorema}

Vamos a comprobar el resultado con \texttt{quickCheck}

\begin{sesion}
ghci>  quickCheck prop_caracterizaGrafoConexo
+++ OK, passed 100 tests.
\end{sesion}      
    
\begin{code}
prop_caracterizaGrafoConexo :: Grafo Int -> Property
prop_caracterizaGrafoConexo g =
    esConexo g ==> and [estanConectados g u v | 
                        u <- vertices g, v <- vertices g]
    && and [estanConectados g u v |
         u <- vertices g, v <- vertices g] ==> esConexo g
\end{code}

\begin{definicion}
  Sea $G=(V,A)$ un grafo. Se define el \textbf{diámetro} de $G$
  como el máximo de las distancias entre los vértices en $V$. Lo 
  denotaremos por $d(G)$.
\end{definicion}

La función \texttt{(diametro g)} devuelve el diámetro del 
grafo \texttt{g}. Por ejemplo,

\begin{sesion}
diametro (grafoCiclo 8)      ==  4.0
diametro (grafoRueda 7)      ==  2.0
diametro grafoPetersen       ==  2.0
diametro grafoMoebiusCantor  ==  4.0
diametro grafoNulo           ==  0.0        
\end{sesion}

\index{\texttt{diametro}}
\begin{code}
diametro :: Ord a => Grafo a -> Double
diametro g
    | null (vertices g) = 0     
    | otherwise =
        maximum (map f (combinaciones 2 (vertices g)))
            where f ([u,v]) = distancia g u v
\end{code}

\begin{definicion}
  Sean $G=(V,A)$ un grafo y $v \in V$. Se define la \textbf{excentricidad}
  de $v$ como el máximo de las distancias entre $v$ y el resto de
  vértices de $G$. La denotaremos por $e(G)$.
\end{definicion}

La función \texttt{(excentricidad g v)} devuelve la excentricidad del 
vértice \texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
excentricidad (grafoCiclo 8) 5      ==  4.0
excentricidad (grafoRueda 7) 4      ==  2.0
excentricidad (grafoRueda 7) 1      ==  1.0
excentricidad grafoPetersen  6      ==  2.0
excentricidad grafoMoebiusCantor 7  ==  4.0
excentricidad grafoNulo 3           ==  0.0              
\end{sesion}

\index{\texttt{excentricidad}}
\begin{code}
excentricidad :: Ord a => Grafo a -> a -> Double
excentricidad g u
    | null (vertices g) = 0
    | otherwise = maximum (map f [(u,v) | v <- vertices g])
         where f (a,b) = distancia g a b
\end{code}

\begin{definicion}
  Sean $G=(V,A)$ un grafo y $v \in V$. Se define el \textbf{radio} de
  $G$ como el mínimo de las excentricidades de sus vértices. Lo
  denotaremos por $r(G)$.
\end{definicion}

La función \texttt{(radio g)} devuelve el radio del
grafo \texttt{g}. Por ejemplo,

\begin{sesion}
radio (grafoCiclo 8)      ==  4.0
radio (grafoRueda 7)      ==  1.0
radio (grafoRueda 7)      ==  1.0
radio grafoPetersen       ==  2.0
radio grafoMoebiusCantor  ==  4.0
radio grafoNulo           ==  0.0
\end{sesion}

\index{\texttt{radio}}
\begin{code}
radio :: Ord a => Grafo a -> Double        
radio g | null (vertices g) = 0
        | otherwise =
            minimum (map (excentricidad g) (vertices g))
\end{code}
    
\begin{definicion}
  Sean $G=(V,A)$ un grafo. Llamamos \textbf{centro} del grafo $G$ al
  conjunto de vértices de excentricidad mínima. A estos vértices se 
  les denomina \textbf{vértices centrales}.
\end{definicion}

La función \texttt{(centro g)} devuelve el centro del grafo
\texttt{g}. Por ejemplo, 

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
centro g = [v | v <- vertices g , r == excentricidad g v]
    where r = radio g
\end{code}
      
\begin{definicion}
  Sean $G=(V,A)$ un grafo. Se llama \textbf{grosor} o \textbf{cintura}
  del grafo $G$ como el máximo de las longitudes de los ciclos de $G$.
\end{definicion}
