\ignora{
\begin{code}
module ConectividadGrafos (esCamino
                          , aristasCamino
                          , verticesCamino
                          , esRecorrido
                          , esCaminoSimple
                          , longitudCamino 
                          , todosCaminos
                          , prop_todosCaminos 
                          , estanConectados
                          , distancia
                          , esGeodesica
                          , esCerrado
                          , esCircuito
                          , esCiclo 
                          -- , prop_conectadosRelEqui
                          -- , componentesConexas
                          -- , esConexo
                          -- , prop_caracterizaGrafoConexo
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
  Sea $G = (V,A)$ un grafo simple y sean $u,v \in V$ dos vértices. Un 
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
esCamino g c = all (aristaEn g) (zip c (tail c))
\end{code}

La función \texttt{(aristasCamino c)} devuelve la lista de las 
aristas recorridas en el camino \texttt{c}.

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
aristasCamino :: Ord a => [a] -> [(a,a)]
aristasCamino c =
    map parOrdenado (zip c (tail c))
    where parOrdenado (u,v) | u <= v = (u,v)
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
esRecorrido :: Ord a => [a] -> Bool      
esRecorrido c =
  aristasCamino c == nub (aristasCamino c)
\end{code}

\begin{definicion}
  Un camino que no repite vértices (y, por tanto, tampoco aristas)
  se llama \textbf{camino simple}.
\end{definicion}

La función \texttt{(esCaminoSimple c)} se verifica si el camino \texttt{c} es
un arco. Por ejemplo,

\begin{sesion}
esCaminoSimple [1..4]       ==  True
esCaminoSimple [1,2,6,1]    ==  True
esCaminoSimple [1,2,3,1,4]  ==  False
esCaminoSimple ['a'..'f']   ==  True
\end{sesion}

\begin{code}
esCaminoSimple :: Ord a => [a] -> Bool
esCaminoSimple c = if (head c /= last c)
                   then (verticesCamino c == c)
                   else (verticesCamino c ++ [last c] == c)
\end{code}

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
longitudCamino :: Num b => [a] -> b
longitudCamino c = genericLength c - 1
\end{code}
    
La función \texttt{(todosCaminos g inicio final)} devuelve una lista con todos
los caminos simples posibles en el grafo \texttt{g} entre los vértices
\texttt{inicio} y \texttt{final}, generados de forma que el primer
camino de la lista sea de longitud mínima.

\begin{sesion}
ghci> todosCaminos (grafoCiclo 7) 1 6
[[1,7,6],[1,2,3,4,5,6]]
ghci> todosCaminos (grafoRueda 7) 2 5
[[2,1,5],[2,3,1,5],[2,7,1,5],[2,1,4,5],[2,3,4,5],[2,1,6,5],
 [2,7,6,5],[2,3,4,1,5],[2,7,6,1,5],[2,3,1,4,5],[2,7,1,4,5],
 [2,1,3,4,5],[2,3,1,6,5],[2,7,1,6,5],[2,1,7,6,5],[2,7,6,1,4,5],
 [2,7,1,3,4,5],[2,3,4,1,6,5],[2,3,1,7,6,5],[2,7,6,1,3,4,5],[2,3,4,1,7,6,5]]
ghci> todosCaminos (creaGrafo [1..4] [(1,2),(2,3)]) 1 4
[]
\end{sesion}

\index{\texttt{todosCaminos}}
\begin{code}
todosCaminos :: Ord a => Grafo a -> a -> a -> [[a]]
todosCaminos g x y = aux [[y]]
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
lista que devuelve la función \texttt{todosCaminos g u v)} es de longitud
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

\begin{nota}
Al hacer la comprobación, vamos a mostrar el orden de los grafos que
genera automáticamente \texttt{quickCheck} para asegurarnos de que la
comprobación sea suficientemente significativa.
\end{nota}    

\begin{sesion}
ghci-- > quickCheckWith (stdArgs {maxSize=15}) prop_todosCaminos
+++ OK, passed 100 tests:
23% 1
18% 3
17% 2
 9% 6
 8% 4
 7% 7
 5% 8
 5% 5
 4% 9
 2% 12
 1% 11
 1% 10
\end{sesion}

\index{\texttt{prop\_todosCaminos}}    
\begin{code}
prop_todosCaminos :: Grafo Int -> Property
prop_todosCaminos g =
  not (esGrafoNulo g) ==>
  collect (orden g) $
  forAll (parDeVertices g)
         (\(x,y) -> let zss = todosCaminos g x y
                    in null zss || longitudCamino (head zss) ==
                                   minimum (map longitudCamino zss))
\end{code}
        
\comentario{Ver ``Sobre caminos y comprobaciones''}

\comentario{Revisado hasta aquí}

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
estanConectados :: Ord a => Grafo a -> a -> a -> Bool
estanConectados g u v | esGrafoNulo g = False
                      | otherwise = not (null (todosCaminos g u v))
\end{code}

\begin{nota}
La función \texttt{(estanConectados g u v)} no necesita calcular todos los
caminos entre \texttt{u} y \texttt{v}. Puesto que Haskell utiliza por        
defecto evaluación perezosa, si existe algún camino entre los dos        
vértices, basta calcular el primero para saber que la lista de todos los 
caminos no es vacía
\end{nota}

\begin{definicion}
  Se define la \textbf{distancia} entre $u$ y $v$ en el grafo $G$
  como la longitud del camino más corto que los une. Si $u$ y $v$
  no están conectados, decimos que la distancia es infinita.
\end{definicion}

La función \texttt{(distancia g u v)} devuelve la distancia entre los
vértices \texttt{u} y \texttt{v} en el grafo \texttt{g}. En caso de 
que los vértices no estén conectados devuelve el valor    
\texttt{Nothing}. Por ejemplo,

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
distancia g u v | estanConectados g u v =
                    Just (longitudCamino (head (todosCaminos g u v)))
                | otherwise = Nothing
\end{code}

\begin{definicion}
  Dado $G=(V,A)$ un grafo, sean $u,v \in V$. Un camino entre
  $u$ y $v$ cuya longitud coincide con la distancia entre los
  vértices se llama \textbf{geodésica} entre $u$ y $v$.
\end{definicion}

La función \texttt{(esGeodesica g c)} se verifica si el camino
\texttt{c} es una geodésica entre sus extremos en el grafo \texttt{g}.

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
    longitudCamino c == fromJust (distancia g u v)
    where u = head c
          v = last c
\end{code}
  
\begin{definicion}
  Un camino en un grafo $G$ se dice \textbf{cerrado} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCerrado g c)} se verifica si el camino \texttt{c} 
es cerrado en el grafo \texttt{g}. Por ejemplo,

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
esCerrado g c = head c == last c
\end{code}
  
\begin{definicion}
  Un recorrido en un grafo $G$ se dice \textbf{circuito} si sus extremos
  son iguales.
\end{definicion}

La función \texttt{(esCircuito g c)} se verifica si la sucesión de 
vértices \texttt{c} es un circuito en el grafo \texttt{g}.
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
esCircuito g c =
    esRecorrido c && esCerrado g c
\end{code}

\begin{definicion}
  Un caminoSimple en un grafo $G$ se dice que es un \textbf{circuito} si 
  sus extremos son iguales.
\end{definicion}

La función \texttt{(esCiclo g c)} se verifica si el camino \texttt{c} 
es un ciclo en el grafo \texttt{g}. Por ejemplo,

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
    esCaminoSimple c && esCerrado g c
\end{code}
    
-- \begin{teorema}
--   Dado un grafo $G$, la relación $u∼v$ (estar conectados por un camino)
--   es una relación de equivalencia.
-- \end{teorema}

-- A continuación, comprobaremos el resultado con \texttt{quickCheck}.
  
-- \begin{sesion}
-- ghci> quickCheck prop_conectadosRelEqui
-- +++ OK, passed 100 tests.
-- \end{sesion}

-- \index{\texttt{prop\_ConectadosRelEqui}}   
-- \begin{code}
-- prop_conectadosRelEqui :: Grafo Int -> Int -> Int -> Int -> Bool
-- prop_conectadosRelEqui g u v w =
--     reflexiva && simetrica && transitiva
--     where reflexiva = estanConectados g u u
--           simetrica =
--               not (estanConectados g u v) || estanConectados g v u
--           transitiva =
--               not (estanConectados g u v && estanConectados g v w)
--                  || estanConectados g u w           
-- \end{code}

-- \begin{definicion}
--   Las clases de equivalencia obtenidas por la relación $∼$, 
--   estar conectados por un camino en un grafo $G$,  inducen subgrafos 
--   en $G$, los vértices y todas las aristas de los caminos que los conectan, 
--   que reciben el nombre de \textbf{componentes conexas por caminos} de $G$.
-- \end{definicion}

-- La función \texttt{(componentesConexas g)} devuelve las componentes 
-- conexas por caminos del grafo \texttt{g}. Por ejemplo,

-- \begin{sesion}
-- ghci> componentesConexas grafoPetersen
-- [[1,2,3,4,5,6,7,8,9,10]]
-- ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3)])
-- [[1,2,3],[4],[5]]
-- ghci> componentesConexas (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
-- [[1,2,3],[4,5]]
-- ghci> componentesConexas grafoNulo
-- []
-- \end{sesion}
      
-- \index{\texttt{componentesConexas}}
-- \begin{code}
-- componentesConexas :: Ord a => Grafo a -> [[a]]
-- componentesConexas g = aux (vertices g)
--       where aux []     = []
--             aux (v:vs) = c: aux (vs \\ c)
--                  where c = filter (estanConectados g v) (v:vs)
-- \end{code}

-- \begin{definicion}
--   Dado un grafo, diremos que es \textbf{conexo} si la relación $~$
--   tiene una única clase de equivalencia en él; es decir, si el grafo
--   tiene una única componente conexa.
-- \end{definicion}

-- La función \texttt{(esConexo g)} se verifica si el grafo \texttt{g}
-- es conexo. Por ejemplo,

-- \begin{sesion}
-- esConexo (completo 5)                      == True
-- esConexo (creaGrafo [1..5] [(1,2),(2,3)])  == False
-- esConexo (creaGrafo [1..3] [(1,2),(2,3)])  == True
-- esConexo grafoNulo                         == False    
-- \end{sesion}

-- \index{\texttt{esConexo}}
-- \begin{code}
-- esConexo :: Ord a => Grafo a -> Bool
-- esConexo g = length (componentesConexas g) == 1
-- \end{code}             

-- \begin{teorema}
--   Sea $G$ un grafo, $G=(V,A)$ es conexo si y solamente si $forall  u,v \in V$ 
--   existe un camino entre $u$ y $v$.
-- \end{teorema}

-- Vamos a comprobar el resultado con \texttt{quickCheck}

-- \begin{sesion}
-- ghci>  quickCheck prop_caracterizaGrafoConexo
-- +++ OK, passed 100 tests.
-- \end{sesion}      
    
-- \begin{code}
-- prop_caracterizaGrafoConexo :: Grafo Int -> Property
-- prop_caracterizaGrafoConexo g =
--     esConexo g ==> and [estanConectados g u v | 
--                         u <- vertices g, v <- vertices g]
--     && and [estanConectados g u v |
--          u <- vertices g, v <- vertices g] ==> esConexo g
-- \end{code}

\begin{definicion}
  Sean $G=(V,A)$ un grafo y $v \in V$. Se define la \textbf{excentricidad}
  de $v$ como el máximo de las distancias entre $v$ y el resto de
  vértices de $G$. La denotaremos por $e(G)$.
\end{definicion}

La función \texttt{(excentricidad g v)} devuelve la excentricidad del 
vértice \texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
excentricidad (grafoCiclo 8) 5      ==  Just 4
excentricidad (grafoRueda 7) 4      ==  Just 2
excentricidad (grafoRueda 7) 1      ==  Just 1
excentricidad grafoPetersen  6      ==  Just 2
ghci> let g = creaGrafo [1,2,3] [(1,2)]
excentricidad g 3                   ==  Nothing
excentricidad grafoNulo 3           ==  Nothing
\end{sesion}

\index{\texttt{excentricidad}}
\begin{code}
excentricidad :: Ord a => Grafo a -> a -> Maybe Int
excentricidad g u
    | esGrafoNulo g = Nothing
    | otherwise = aux [fromJust (d v) | v <- vs, isJust (d v)]
    where vs = vertices g \\ [u]
          d w = distancia g u w
          aux [] = Nothing
          aux ds = Just (maximum ds)
\end{code}

\begin{definicion}
  Sea $G=(V,A)$ un grafo. Se define el \textbf{diámetro} de $G$
  como el máximo de las distancias entre los vértices en $V$. Lo 
  denotaremos por $d(G)$.
\end{definicion}

La función \texttt{(diametro g)} devuelve el diámetro del 
grafo \texttt{g}. Por ejemplo,

\begin{sesion}
diametro (grafoCiclo 8)      ==  Just 4
diametro (grafoRueda 7)      ==  Just 2
diametro grafoPetersen       ==  Just 2
ghci> let g = creaGrafo [1,2,3] [(1,2)]
diametro g                   ==  Just 1
diametro grafoNulo           ==  Nothing       
\end{sesion}

\index{\texttt{diametro}}
\begin{code}
diametro :: Ord a => Grafo a -> Maybe Int           
diametro g | esGrafoNulo g = Nothing
           | otherwise = aux [excentricidad g v | v <- vs]
    where vs = vertices g
          aux [] = Nothing
          aux xs = Just (maximum (map fromJust (filter isJust xs)))
\end{code}

\begin{definicion}
  Sean $G=(V,A)$ un grafo y $v \in V$. Se define el \textbf{radio} de
  $G$ como el mínimo de las excentricidades de sus vértices. Lo
  denotaremos por $r(G)$.
\end{definicion}

La función \texttt{(radio g)} devuelve el radio del
grafo \texttt{g}. Por ejemplo,

\begin{sesion}
radio (grafoCiclo 8)      ==  Just 4
radio (grafoRueda 7)      ==  Just 1
radio grafoPetersen       ==  Just 2
ghci> let g = creaGrafo [1,2,3] [(1,2)]
radio g                   ==  Just 1
radio grafoNulo           ==  Nothing
\end{sesion}

\index{\texttt{radio}}
\begin{code}
radio :: Ord a => Grafo a -> Maybe Int        
radio g | esGrafoNulo g = Nothing
        | otherwise = aux [excentricidad g v | v <- vs]
    where vs = vertices g
          aux [] = Nothing
          aux xs = Just (minimum (map fromJust (filter isJust xs)))
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
