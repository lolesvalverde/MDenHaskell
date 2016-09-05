\ignora{
\begin{code}
module Conectividad             ( estarConectadosCamino
                                , prop_conectadosRelEqui
                                , componentesConexas
                                , esConexo
                                , prop_caracterizaGrafoConexo
                                , diametro
                                , excentricidad
                                , radio
                                , centro
                                , grosor
                                , prop_ConexionIsomorfismo1
                                , prop_ConexionIsomorfismo2
                                ) where
  
import Data.List                ( (\\)
                                 , nub
                                 , sort
                                 )
import Data.Maybe               ( fromJust)
import Test.QuickCheck          ( Gen
                                , Property
                                , (==>)
                                , elements
                                , forAll
                                , quickCheck
                                , quickCheckWith
                                , stdArgs
                                , maxDiscardRatio
                                , sample
                                , arbitrary
                                )
import Conjuntos                ( conjuntosIguales
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
                                , grafoAmistad
                                , completo
                                , bipartitoCompleto
                                , grafoRueda
                                , grafoEstrella
                                , grafoHeawood
                                , grafoMcGee
                                , grafoTutteCoxeter
                                , grafoPetersen
                                , grafoMoebiusCantor
                                )
import DefinicionesYPropiedades ( orden
                                , esCompleto
                                )
import Funciones                ( imagen
                                , imagenConjunto
                                )
import Morfismos                ( isomorfos
                                , isomorfismos
                                , esInvariantePorIsomorfismos
                                )
import Caminos                  ( todosArcosBA
                                , longitudCamino
                                , esRecorrido
                                , esCamino
                                , esCaminoSimple
                                , estanConectados
                                , distancia
                                , esAciclico
                                , todosCiclos
                                )
\end{code}
}

\subsection{Estar conectados por un camino}

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

\subsection{Componentes conexas de un grafo}

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
componentesConexas g
  | esGrafoNulo g = []
  | esCompleto g  = [vertices g]
  | otherwise     =
      clasesEquivalencia (vertices g) (estarConectadosCamino g)
\end{code}

\comentario{Se puede hacer una primera definición de
  \texttt{componentesConexas} directa (sin comprobar que es completo),
  comprobar su equivalencia y comparar su eficiencia.}

La función \texttt{(numeroComponentes g)} devuelve el número de componentes
conexas del grafo \texttt{g}.

\index{texttt{numeroComponentesConexas}}
\begin{code}
-- Ejemplos
-- >>> numeroComponentes (creaGrafo [1..5] [(1,2),(2,3),(4,5)])
-- 2
-- >>> numeroComponentes (creaGrafo [1..4] [(1,2),(2,2),(3,4)])
-- 2
numeroComponentes :: Ord a => Grafo a -> Int
numeroComponentes g
    | null (aristas g) = orden g
    | esCompleto g     = 1
    | otherwise        = length (componentesConexas g)
\end{code}

\comentario{La definición de \texttt{numeroComponentes} se puede simplificar.}

\subsection{Grafos conexos}

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
  Sea $G$ un grafo, $G = (V,A)$ es conexo si y solamente si
  $\forall u, v \in V$ existe un camino entre $u$ y $v$.
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

\subsection{Excentricidad}

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
  | orden g == 1              = Just 0
  | Nothing `elem` distancias = Nothing
  | otherwise                 = maximum distancias
  where distancias = [distancia g u v | v <- vertices g \\ [u]]
\end{code}

La función \texttt{(excentricidades g)} devuelve la lista ordenada 
de las excentricidades de los vértices del grafo \texttt{g}.

\index{\texttt{excentricidades}}
\begin{code}
-- | Ejemplos
-- >>> excentricidades (creaGrafo [1..3] [(1,2),(2,3),(3,3)])
-- [Just 1,Just 2,Just 2]
-- >>> excentricidades (creaGrafo [1..3] [(1,2),(3,3)])
-- [Nothing,Nothing,Nothing]
excentricidades :: Ord a => Grafo a -> [Maybe Int]
excentricidades g = sort (map (excentricidad g) (vertices g))
\end{code}

\subsection{Diámetro}

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

\subsection{Radio}

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

\subsection{Centro}

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

\subsection{Grosor}

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
-- >>> grosor grafoPetersen
-- Just 5
-- >>> grosor grafoMoebiusCantor
-- Just 6
-- >>> grosor grafoHeawood
-- Just 6
-- >>> grosor grafoMcGee
-- Just 7
-- >>> grosor grafoTutteCoxeter
-- Just 8
grosor :: Ord a => Grafo a -> Maybe Int
grosor g
  | esAciclico g = Nothing
  | otherwise    = Just (minimum [longitudCamino (head yss)
                                 | x <- vertices g
                                 , let yss = todosCiclos g x
                                 , not (null yss)])
\end{code}

\subsubsection{Propiedades del grosor de los grafos}

\begin{teorema}
  El grosor del grafo ciclo de orden $n$, $C_n$, es $\infty$, si $n < 3$
  y 3 en caso contrario.
\end{teorema}

La propiedad se expresa por

\begin{code}
prop_grosor_grafoCiclo :: Int -> Bool
prop_grosor_grafoCiclo n = 
  grosor (grafoCiclo n) == if n < 3
                           then Nothing
                           else Just n
\end{code}

Su comprobación para $n \leq 30$ es
\begin{sesion}
ghci> all prop_grosor_grafoCiclo [1..30]
True
\end{sesion}

\begin{teorema}
  El grosor del grafo amistad de orden $n$ es 3, para todo $n$.
\end{teorema}

\begin{code}
prop_grosor_grafoAmistad :: Int -> Bool
prop_grosor_grafoAmistad n =
  grosor (grafoAmistad n) == Just 3
\end{code}

Su comprobación para $n \leq 30$ es
\begin{sesion}
ghci> all prop_grosor_grafoAmistad [1..30]
True
\end{sesion}

\begin{teorema}
  El grosor del grafo completo de orden $n$, $K_n$, es $\infty$ si $n < 3$
  y 3 en caso contrario.
\end{teorema}

La propiedad se expresa por

\begin{code}
prop_grosor_completo :: Int -> Bool
prop_grosor_completo n = 
  grosor (completo n) == if n < 3
                         then Nothing
                         else Just 3
\end{code}

Su comprobación para $n \leq 30$ es
\begin{sesion}
ghci> all prop_grosor_completo [1..30] 
True
\end{sesion}

\begin{teorema}
  El grosor del grafo bipartito completo de orden, $K_{m,n}$, es
  $\infty$ si $m = 1$ ó $n = 1$ y es 4 en caso contrario.
  y 3 en caso contrario.
\end{teorema}

La propiedad se expresa por

\begin{code}
prop_grosor_bipartitoCompleto :: Int -> Int -> Bool
prop_grosor_bipartitoCompleto m n = 
  grosor (bipartitoCompleto m n) == if m == 1 || n == 1
                                    then Nothing
                                    else Just 4
\end{code}

Su comprobación para $1 \leq m \leq n \leq 15$ es
\begin{sesion}
ghci> and [prop_grosor_bipartitoCompleto m n | m <- [1..15], n <- [m..15]]
True
\end{sesion}

\begin{teorema}
  El grosor del grafo rueda de orden $n$, $W_n$ es $\infty$ si $n < 3$
  y 3 en caso contrario.
\end{teorema}

La propiedad se expresa por

\begin{code}
prop_grosor_grafoRueda :: Int -> Bool
prop_grosor_grafoRueda n = 
  grosor (grafoRueda n) == if n < 3
                           then Nothing
                           else Just 3     
\end{code}

Su comprobación para $1 \leq n \leq 30$ es
\begin{sesion}
ghci> all prop_grosor_grafoRueda [1..30]
True
\end{sesion}

\subsection{Propiedades e invariantes por isomorfismos}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ grafos isomorfos con $\phi: V \to V'$ un
  isomorfismo. Entonces, dados $u,v \in V$, $u ∼ v$ si y solamente si
  $\phi(u) ∼ \phi(v)$.
\end{teorema}

La comprobación del teorema con QuickCheck es:

\begin{sesion}
ghci> quickCheckWith stdArgs {maxDiscardRatio = 20} prop_ConexionIsomorfismo1
+++ OK, passed 100 tests.
\end{sesion}

donde
    
\index{\texttt{prop\_ConexionIsomorfismo1}}
\begin{code}
prop_ConexionIsomorfismo1 :: Grafo Int -> Grafo Int -> Property
prop_ConexionIsomorfismo1 g h =
  isomorfos g h ==>
  and [ec g u v == ec h (imagen phi u) (imagen phi v)
      | u <- vs
      , v <- vs
      , phi <- isomorfismos g h]
      where vs = vertices g
            ec = estanConectados
\end{code}

\comentario{La propiedad \texttt{prop\_ConexionIsomorfismo1} no es buena para
  comprobarla con QuickCheck porque la mayoría de los pares de grafos generados
  serán no isomorfos.}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ grafos isomorfos con $\phi: V \to V'$ 
  un isomorfismo. Entonces, $\phi$ lleva cada componente conexa de $G$
  en una componente conexa de $G'$.
\end{teorema}

La comprobación del teorema con QuickCheck es:

\begin{sesion}
ghci> quickCheckWith stdArgs {maxDiscardRatio = 15} prop_ConexionIsomorfismo2
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ConexionIsomorfismo2}}
\begin{code}
prop_ConexionIsomorfismo2 :: Grafo Int -> Grafo Int -> Property
prop_ConexionIsomorfismo2 g h =
  isomorfos g h ==>
  and [conjuntosIguales cch (aux f) | f <- isomorfismos g h]
      where cch = componentesConexas h
            ccg = componentesConexas g
            aux f = map (sort . imagenConjunto f) ccg
\end{code}

\comentario{La propiedad \texttt{prop\_ConexionIsomorfismo2} no es buena para
  comprobarla con QuickCheck porque la mayoría de los pares de grafos generados
  serán no isomorfos.}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ grafos isomorfos con $\phi: V \to V'$ 
  un isomorfismo. Entonces, $G$ y $G'$ tienen el mismo número de  
  componentes conexas.
\end{teorema}

La comprobación del teorema con QuickCheck es:

\begin{sesion}
ghci> quickCheckWith stdArgs {maxDiscardRatio = 15} prop_ConexionIsomorfismo3
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ConexionIsomorfismo3}}
\begin{code}
prop_ConexionIsomorfismo3 :: Grafo Int -> Grafo Int -> Property
prop_ConexionIsomorfismo3 g h =
  isomorfos g h ==>
  numeroComponentes g == numeroComponentes h
\end{code}

\comentario{La propiedad \texttt{prop\_ConexionIsomorfismo3} no es buena para
  comprobarla con QuickCheck porque la mayoría de los pares de grafos generados
  serán no isomorfos.}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que el diámetro de $G$ es igual 
  al diámetro de $G'$; es decir, el diámetro de un grafo es un  
  invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos diametro)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que el radio de $G$ es igual 
  al radio de $G'$; es decir, el radio de un grafo es un  
  invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos radio)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que el grosor de $G$ es igual 
  al grosor de $G'$; es decir, el grosor de un grafo es un  
  invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos grosor)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que el centro de $G$ es igual 
  al centro de $G'$; es decir, el centro de un grafo es un  
  invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos centro)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que el número de componentes  
  conexas de  $G$ es igual al número de las de $G'$; es decir, el 
  número de componentes conexas de un grafo es un invariante por 
  isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos numeroComponentes)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que los valores que toman las
  excentricidades de los vértices $G$ es igual al de los vértices de 
  $G'$; es decir, el conjunto de valores que toman las excentricidades 
  de los vértices es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheck (esInvariantePorIsomorfismos excentricidades)
+++ OK, passed 100 tests.
\end{sesion}

\ignora{
  La validación es

  > doctest Conectividad.lhs 
  Examples: 335  Tried: 335  Errors: 0  Failures: 0
}
