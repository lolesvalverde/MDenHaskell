\ignora{
\begin{code}
module Morfismos ( esMorfismo
                  , morfismos
                  , esIsomorfismo
                  , isomorfismos
                  , isomorfos
                  -- , prop_ordenInvariante
                  -- , prop_tamañoInvariante
                  -- , prop_secuenciaGradosInvariante
                  , esAutomorfismo
                  , automorfismos                  
                  ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import ConjuntosRelacionesYFunciones
import DefinicionesYPropiedades 
    
import Test.QuickCheck
import Data.List
\end{code}
}

Llegados a este punto, es importante resaltar que un grafo se define como una
entidad matemática abstracta; es evidente que lo importante de un grafo no son
los nombres de sus vértices ni su representación gráfica. La propiedad que
caracteriza a un grafo es la forma en que sus vértices están unidos por las
aristas.

A priori, los siguientes grafos pueden parecer distintos:

\begin{minipage}{.5\textwidth}
  \centering
  \begin{tikzpicture}
  \SetVertexNoLabel
  \tikzstyle{every node} = [draw, 
                            shape        = circle,
                            ball color   = orange,
                            minimum size = 24pt]
  \GraphInit [vstyle = Shade]
  \node (a) at (0:1.5){a};
  \node (b) at (72:1.5){b};
  \node (c) at (144:1.5){c};
  \node (d) at (216:1.5){d};
  \node (e) at (288:1.5){e};
  \Edge (a)(b)
  \Edge (a)(c)
  \Edge (a)(e)
  \Edge (b)(d)
  \Edge (c)(d)
  \Edge (d)(e)
  \end{tikzpicture}
\end{minipage}
\begin{minipage}{.5\textwidth}
  \begin{tikzpicture}
  \SetVertexNoLabel
  \tikzstyle{every node} = [draw, 
                            shape        = circle,
                            ball color   = green,
                            minimum size = 24pt]
  \GraphInit [vstyle = Shade]
  \node (d) at (0:1.5){1};
  \node (b) at (72:1.5){2};
  \node (a) at (144:1.5){3};
  \node (e) at (216:1.5){4};
  \node (c) at (288:1.5){5};
  \tikzset{EdgeStyle/.append style = {bend left}}
  \Edge (a)(b)
  \Edge (a)(c)
  \Edge (b)(d)
  \Edge (c)(d)
  \tikzset{EdgeStyle/.append style = {bend right}}
  \Edge (a)(e)
  \Edge (d)(e)
  \end{tikzpicture}
\end{minipage}

Sin embargo, ambos grafos son grafos de cinco vértices que tienen las mismas
relaciones de vecindad entre sus nodos. En esta sección estudiaremos estas
relaciones que establecen las aristas entre los vértices de un grafo y
presentaremos algoritmos que nos permitan identificar cuándo dos grafos se
pueden relacionar mediante aplicaciones entre sus vértices cumpliendo ciertas
caracterísicas.

\subsection{Morfismos}

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un \textbf{morfismo}
  entre $G$ y $G'$ es una función $\phi: V \to V'$ que conserva las
  adyacencias.
\end{definicion}

La función \texttt{(esMorfismo g h vvs)} se verifica si la función representada
por \texttt{vvs} es un morfismo entre los grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> let g1 = creaGrafo [1,2,3] [(1,2),(2,3)]
ghci> let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
ghci> esMorfismo g1 g2 [(1,4),(2,6),(3,5)]
True
ghci> esMorfismo g1 g2 [(1,4),(2,5),(3,6)]
False
ghci> esMorfismo g1 g2 [(1,4),(2,6),(3,5),(7,9)]
False
\end{sesion}

\begin{code}
esMorfismo :: (Ord a,Ord b) => Grafo a -> Grafo b ->
              Funcion a b -> Bool
esMorfismo g1 g2 f =
  esFuncion (vertices g1) (vertices g2) f &&
  conservaAdyacencia g1 g2 f
\end{code}

La función \texttt{(morfismos g h)} devuelve todos los
posibles morfismos entre los grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> morfismos (grafoCiclo 3)
                (creaGrafo [4..6] [(4,5),(4,6),(5,6)])
[[(1,4),(2,5),(3,6)],[(1,4),(2,6),(3,5)],[(1,5),(2,4),(3,6)],
 [(1,5),(2,6),(3,4)],[(1,6),(2,4),(3,5)],[(1,6),(2,5),(3,4)]]
ghci> morfismos (bipartitoCompleto 1 2) (grafoCiclo 3)
[[(1,1),(2,2),(3,2)],[(1,1),(2,2),(3,3)],[(1,1),(2,3),(3,2)],
 [(1,1),(2,3),(3,3)],[(1,2),(2,1),(3,1)],[(1,2),(2,1),(3,3)],
 [(1,2),(2,3),(3,1)],[(1,2),(2,3),(3,3)],[(1,3),(2,1),(3,1)],
 [(1,3),(2,1),(3,2)],[(1,3),(2,2),(3,1)],[(1,3),(2,2),(3,2)]]
\end{sesion}

\index{\texttt{morfismos}}
\begin{code}
morfismos :: (Ord a, Ord b) => Grafo a -> Grafo b -> [[(a,b)]]
morfismos g h =
  [f | f <- funciones (vertices g) (vertices h)
     , esMorfismo g h f]
\end{code}

\subsection{Isomorfismos}

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un
  \textbf{isomorfismo} entre $G$ y $G'$ es un morfismo biyectivo cuyo inverso
  es morfismo entre $G'$ y $G$.
\end{definicion}

La función \texttt{(esIsomorfismo g h f)} se verifica si la aplicación
\texttt{f} es un isomorfismo entre los grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> esIsomorfismo (grafoCiclo 3)
                    (creaGrafo ['a'..'c']
                               [('a','b'),('a','c'),('b','c')])
                    [(1,'a'),(2,'b'),(3,'c')]
True
ghci> esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,1),(2,3),(3,2)]
False
ghci> esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,3),(2,2),(2,2)] 
False
\end{sesion}

\index{\texttt{esIsomorfismo}}
\begin{code}
esIsomorfismo :: (Ord a,Ord b) =>
                 Grafo a -> Grafo b -> Funcion a b -> Bool
esIsomorfismo g h f =
  esBiyectiva vs1 vs2 f &&
  conservaAdyacencia g h f
  where vs1 = vertices g
        vs2 = vertices h      
\end{code}

La función \texttt{(isomorfismos1 g h)} devuelve todos los isomorfismos posibles
entre los grafos \texttt{g} y \texttt{h}. Por ejemplo,

\begin{sesion}
ghci> isomorfismos1 (bipartitoCompleto 1 2) (grafoCiclo 3)
[]
ghci> isomorfismos1 (bipartitoCompleto 1 2) 
                   (creaGrafo "abc" [('a','b'),('b','c')])
[[(1,'b'),(2,'a'),(3,'c')],[(1,'b'),(2,'c'),(3,'a')]]
ghci> isomorfismos1 (grafoCiclo 4) 
                   (creaGrafo [5..8] [(5,7),(5,8),(6,7),(6,8)])
[[(1,5),(2,7),(3,6),(4,8)],[(1,5),(2,8),(3,6),(4,7)],
 [(1,6),(2,7),(3,5),(4,8)],[(1,6),(2,8),(3,5),(4,7)],
 [(1,7),(2,5),(3,8),(4,6)],[(1,7),(2,6),(3,8),(4,5)],
 [(1,8),(2,5),(3,7),(4,6)],[(1,8),(2,6),(3,7),(4,5)]
\end{sesion}

\index{\texttt{isomorfismos}}
\begin{code}
isomorfismos1 :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos1 g h =
  [f | f <- biyecciones vs1 vs2 , conservaAdyacencia g h f]
  where vs1 = vertices g
        vs2 = vertices h
\end{code}

\begin{definicion}
  Dos grafos $G$ y $H$ se dicen \textbf{isomorfos} si existe algún isomorfismo
  entre ellos.
\end{definicion}

La función \texttt{isomorfos1 g h} se verifica si los grafos \texttt{g} y
\texttt{h} son isomorfos. Por ejemplo,

\begin{sesion}
ghci> isomorfos1 (grafoRueda 4) (completo 4)
True
ghci> isomorfos1 (grafoRueda 5) (completo 5)
False
ghci> isomorfos1 (grafoEstrella 2) (bipartitoCompleto 1 2)
True
ghci> isomorfos1 (grafoCiclo 5) (bipartitoCompleto 2 3)
False
\end{sesion}

\index{\texttt{isomorfos}}
\begin{code}
isomorfos1 :: (Ord a,Ord b) => Grafo a -> Grafo b -> Bool
isomorfos1 g = not . null . isomorfismos1 g 
\end{code}

\begin{nota}
Al tener Haskell una evaluación perezosa, la función 
\texttt{(isomorfos g h)} no necesita generar todos los isomorfismos
entre los grafos \texttt{g} y \texttt{h}.
\end{nota}

\begin{definicion}
  Sea $G = (V,A)$ un grafo. Un \textbf{invariante por isomorfismos} 
  de $G$ es una propiedad de $G$ que tiene el mismo valor para todos 
  los grafos que son isomorfos a él.
\end{definicion}

\begin{code}
esInvariantePorIsomorfismos ::
  Eq a => (Grafo Int -> a) -> Grafo Int -> Grafo Int -> Bool
esInvariantePorIsomorfismos p g h = 
  isomorfos g h --> (p g == p h)
      where (-->) = (<=)
\end{code}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V\to V'$ un 
  isomorfismo. Entonces, se verifica que $|V(G)| = |V(G')|$; es decir,
  el orden de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=10}) 
                     (esInvariantePorIsomorfismos orden)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$ dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, se verifica que $|A(G)|=|A(G')|$; es decir,
  el tamaño de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=7})
                     (esInvariantePorIsomorfismos tamaño)
+++ OK, passed 100 tests.
\end{sesion}

\begin{teorema}
  Sean $G = (V,A)$ y $G' = (V',A')$  dos grafos y $\phi: V \to V'$ un 
  isomorfismo. Entonces, tienen la misma secuencia de grados; es decir,
  la secuencia de grados de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con QuickCheck.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=7})
                     (esInvariantePorIsomorfismos secuenciaGrados)
+++ OK, passed 100 tests.
\end{sesion}

A partir de las propiedades que hemos demostrado de los isomorfismos,
vamos a dar otra definición equivalente de las funciones 
\texttt{(isomorfismos1 g h)} y \texttt{(isomorfos1 g h)}.

\begin{code}
isomorfismos2 ::
    (Ord a, Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos2 g h
     | orden g  /= orden h  = []
     | tamaño g /= tamaño h = []
     | secuenciaGrados g /= secuenciaGrados h = []
     | otherwise = [f | f <- biyecciones vs1 vs2 , conservaAdyacencia g h f]
     where vs1 = vertices g
           vs2 = vertices h   

isomorfos2 ::
    (Ord a, Ord b) => Grafo a -> Grafo b -> Bool
isomorfos2 g =
    not. null . isomorfismos2 g
\end{code}

Vamos a comparar la eficiencia entre ambas definiciones

\begin{sesion}
ghci> let n = 6 in (length (isomorfismos1 (completo n) (completo n)))
720
(0.29 secs, 32,272,208 bytes)
ghci> let n = 6 in (length (isomorfismos2 (completo n) (completo n)))
720
(0.30 secs, 36,592,648 bytes)

ghci> let n = 6 in (length (isomorfismos1 (grafoCiclo n) (grafoCiclo n)))
12
(0.04 secs, 0 bytes)
ghci> let n = 6 in (length (isomorfismos2 (grafoCiclo n) (grafoCiclo n)))
12
(0.04 secs, 0 bytes)

ghci> length (isomorfismos1 (grafoCiclo 6) (completo 7))
0
(0.00 secs, 0 bytes)
ghci> length (isomorfismos2 (grafoCiclo 6) (completo 7))
0
(0.01 secs, 0 bytes)

ghci> let n = 6 in (isomorfos1 (completo n) (completo n))
True
(0.01 secs, 0 bytes)
ghci> let n = 6 in (isomorfos2 (completo n) (completo n))
True
(0.01 secs, 0 bytes)

ghci> isomorfos1 (grafoCiclo 100) (grafoRueda 100)
False

ghci> isomorfos2 (grafoCiclo 100) (grafoRueda 100)
False
(0.01 secs, 0 bytes)
\end{sesion}

\begin{nota}
Cuando los grafos son , comprobar que tienen el mismo número de
vértices, el mismo número de aristas y la misma secuencia gráfica no requiere
mucho tiempo ni espacio, dando lugar a costes muy similares entre los dos pares
de definiciones. Sin embargo, cuando los grafos no son isomorfos y fallan en
alguna de las propiedades, el resultado es inmediato con las segundas
definiciones.

A partir de ahora utilizaremos la función \texttt{(isomorfismos2 g h)}  
para calcular los isomorfismos entre dos grafos y la función     
\texttt{(isomorfos g h)} para determinar si dos grafos son isomorfos, de
modo que las renombraremos por \texttt{(isomorfismos g h)} y
\texttt{(isomorfismos g h)} respectivamente.

\begin{code}
isomorfismos :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos = isomorfismos2

isomorfos :: (Ord a,Ord b) => Grafo a -> Grafo b -> Bool
isomorfos = isomorfos2
\end{code}
\end{nota}

\subsection{Automorfismos}

\begin{definicion}
  Dado un grafo simple $G = (V,A)$, un \textbf{automorfismo}
  de $G$ es un isomorfismo de $G$ en sí mismo.
\end{definicion}

La función \texttt{(esAutomorfismo g f)} se verifica si la aplicación
\texttt{f} es un automorfismo de \texttt{g}.

\begin{sesion}
ghci> esAutomorfismo (bipartitoCompleto 1 2) [(1,2),(2,3),(3,1)]
False
ghci> esAutomorfismo (bipartitoCompleto 1 2) [(1,1),(2,3),(3,2)]
True
ghci> esAutomorfismo (grafoCiclo 4) [(1,2),(2,3),(3,4),(4,1)]
True
\end{sesion}

\index{\texttt{esAutomorfismo}}
\begin{code}
esAutomorfismo :: Ord a => Grafo a -> Funcion a a -> Bool
esAutomorfismo g = esIsomorfismo g g 
\end{code}

La función \texttt{(automorfismos g)} devuelve la lista de todos los posibles
automorfismos en \texttt{g}. Por ejemplo,

\begin{sesion}
ghci> automorfismos (grafoCiclo 4)
[[(1,1),(2,2),(3,3),(4,4)],[(1,1),(2,4),(3,3),(4,2)],
 [(1,2),(2,1),(3,4),(4,3)],[(1,2),(2,3),(3,4),(4,1)],
 [(1,3),(2,2),(3,1),(4,4)],[(1,3),(2,4),(3,1),(4,2)],
 [(1,4),(2,1),(3,2),(4,3)],[(1,4),(2,3),(3,2),(4,1)]]
ghci> automorfismos (completo 4)
[[(1,1),(2,2),(3,3),(4,4)],[(1,1),(2,2),(3,4),(4,3)],
 [(1,1),(2,3),(3,2),(4,4)],[(1,1),(2,3),(3,4),(4,2)],
 [(1,1),(2,4),(3,2),(4,3)],[(1,1),(2,4),(3,3),(4,2)],
 [(1,2),(2,1),(3,3),(4,4)],[(1,2),(2,1),(3,4),(4,3)],
 [(1,2),(2,3),(3,1),(4,4)],[(1,2),(2,3),(3,4),(4,1)],
 [(1,2),(2,4),(3,1),(4,3)],[(1,2),(2,4),(3,3),(4,1)],
 [(1,3),(2,1),(3,2),(4,4)],[(1,3),(2,1),(3,4),(4,2)],
 [(1,3),(2,2),(3,1),(4,4)],[(1,3),(2,2),(3,4),(4,1)],
 [(1,3),(2,4),(3,1),(4,2)],[(1,3),(2,4),(3,2),(4,1)],
 [(1,4),(2,1),(3,2),(4,3)],[(1,4),(2,1),(3,3),(4,2)],
 [(1,4),(2,2),(3,1),(4,3)],[(1,4),(2,2),(3,3),(4,1)],
 [(1,4),(2,3),(3,1),(4,2)],[(1,4),(2,3),(3,2),(4,1)]]
ghci> automorfismos (grafoRueda 5)
[[(1,1),(2,2),(3,3),(4,4),(5,5)],[(1,1),(2,2),(3,5),(4,4),(5,3)],
 [(1,1),(2,3),(3,2),(4,5),(5,4)],[(1,1),(2,3),(3,4),(4,5),(5,2)],
 [(1,1),(2,4),(3,3),(4,2),(5,5)],[(1,1),(2,4),(3,5),(4,2),(5,3)],
 [(1,1),(2,5),(3,2),(4,3),(5,4)],[(1,1),(2,5),(3,4),(4,3),(5,2)]]
\end{sesion}

\index{\texttt{automorfismos}}
\begin{code}
automorfismos :: Ord a => Grafo a -> [Funcion a a] 
automorfismos g = isomorfismos1 g g 
\end{code}

\begin{nota}
  Cuando trabajamos con automorfismos, es mejor utilizar en su definición la
  función \texttt{isomorfismos} en vez de \texttt{isomorfismos2}, pues ser
  isomorfos es una relación reflexiva, es decir, un grafo siempre es isomorfo a
  sí mismo.
\end{nota}
