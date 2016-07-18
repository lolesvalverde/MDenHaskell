
\ignora{
\begin{code}
module Morfismos ( esMorfismo
                  , morfismos
                  , esIsomorfismo
                  , isomorfismos
                  , isomorfos
                  , esAutomorfismo
                  , automorfismos
                  , prop_ordenInvariante
                  , prop_tamañoInvariante
                  , prop_secuenciaGradosInvariante
                  ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import ConjuntosRelacionesYFunciones
    
import Test.QuickCheck
import Data.List
import DefinicionesYPropiedades 
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

La función \texttt{(esMorfismo g h vvs)} se verifica si la
función representada por \texttt{vvs} es un morfismo entre los 
grafos \texttt{g} y \texttt{h}.

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
esMorfismo :: (Eq a,Ord b) => Grafo a -> Grafo b -> Funcion a b -> Bool
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
morfismos :: (Eq a, Ord b) => Grafo a -> Grafo b -> [[(a,b)]]
morfismos g h =
  [f | f <- funciones (vertices g) (vertices h)
     , esMorfismo g h f]
\end{code}

\subsection{Isomorfismos}

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un
  \textbf{isomorfismo} entre $G$ y $G'$ es un morfismo biyectivo
  cuyo inverso es morfismo entre $G'$ y $G$.
\end{definicion}

La función \texttt{(esIsomorfismo g h f)} se verifica si la
aplicación \texttt{f} es un isomorfismo entre los 
grafos \texttt{g} y \texttt{h}.

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
  esMorfismo g h f &&
  esBiyectiva vs1 vs2 f &&
  esMorfismo h g (inversa f)
  where vs1 = vertices g
        vs2 = vertices h      
\end{code}

La función \texttt{(isomorfismos g h)} devuelve todos los isomorfismos posibles
entre los grafos \texttt{g} y \texttt{h}. Por ejemplo,

\begin{sesion}
ghci> isomorfismos (bipartitoCompleto 1 2) (grafoCiclo 3)
[]
ghci> isomorfismos (bipartitoCompleto 1 2) 
                   (creaGrafo "abc" [('a','b'),('b','c')])
[[(1,'b'),(2,'a'),(3,'c')],[(1,'b'),(2,'c'),(3,'a')]]
ghci> isomorfismos (grafoCiclo 4) 
                   (creaGrafo [5..8] [(5,7),(5,8),(6,7),(6,8)])
[[(1,5),(2,7),(3,6),(4,8)],[(1,5),(2,8),(3,6),(4,7)],
 [(1,6),(2,7),(3,5),(4,8)],[(1,6),(2,8),(3,5),(4,7)],
 [(1,7),(2,5),(3,8),(4,6)],[(1,7),(2,6),(3,8),(4,5)],
 [(1,8),(2,5),(3,7),(4,6)],[(1,8),(2,6),(3,7),(4,5)]
\end{sesion}

\index{\texttt{isomorfismos}}
\begin{code}
isomorfismos :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos g h =
  [f | f <- funciones vs1 vs2 , esIsomorfismo g h f]
  where vs1 = vertices g
        vs2 = vertices h
\end{code}

\begin{definicion}
  Dos grafos $G$ y $H$ se dicen \textbf{isomorfos} si existe algún isomorfismo
  entre ellos.
\end{definicion}

La función \texttt{isomorfos g h} se verifica si los grafos \texttt{g} y
\texttt{h} son isomorfos. Por ejemplo,

\begin{sesion}
ghci> isomorfos (grafoRueda 4) (completo 4)
True
ghci> isomorfos (grafoRueda 5) (completo 5)
False
ghci> isomorfos (grafoEstrella 2) (bipartitoCompleto 1 2)
True
ghci> isomorfos (grafoCiclo 5) (bipartitoCompleto 2 3)
False
\end{sesion}

\index{\texttt{isomorfos}}
\begin{code}
isomorfos :: (Ord a,Ord b) => Grafo a -> Grafo b -> Bool
isomorfos g = not . null . isomorfismos g 
\end{code}

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

La función \texttt{automorfismos g} devuelve la lista de todos los
posibles automorfismos en \texttt{g}. Por ejemplo,

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
automorfismos g = isomorfismos g g 
\end{code}

\subsection{Invariantes por isomorfismos}

\begin{definicion}
  Sea $G=(V,A)$ un grafo. Un \textbf{invariante por isomorfismos} 
  de $G$ es una propiedad de $G$ que tiene el mismo valor para todos 
  los grafos que son isomorfos a él.
\end{definicion}

\begin{teorema}
  Sean $G=(V,A)$ y $G'=(V',A')$  dos grafos y $\phi:V\to V'$ un 
  isomorfismo. Entonces, se verifica que $|V(G)|=|V(G')|$; es decir,
  el orden de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con \texttt{QuickCheck}.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=7}) prop_ordenInvariante
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_ordenInvariante}}
\begin{code}
prop_ordenInvariante
  :: Grafo Int -> Grafo Int -> Bool
prop_ordenInvariante g h =
    not (isomorfos g h) || orden g == orden h
\end{code}

\begin{teorema}
  Sean $G=(V,A)$ y $G'=(V',A')$  dos grafos y $\phi:V\to V'$ un 
  isomorfismo. Entonces, se verifica que $|A(G)|=|A(G')|$; es decir,
  el tamaño de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con \texttt{QuickCheck}.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=7}) prop_tamañoInvariante
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_tamañoInvariante}}
\begin{code}
prop_tamañoInvariante
  :: Grafo Int -> Grafo Int -> Bool
prop_tamañoInvariante g h =
    not (isomorfos g h) || tamaño g == tamaño h
\end{code}

\begin{teorema}
  Sean $G=(V,A)$ y $G'=(V',A')$  dos grafos y $\phi:V\to V'$ un 
  isomorfismo. Entonces, tienen la misma secuencia de grados; es decir,
  la secuencia de grados de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con \texttt{QuickCheck}.

\begin{sesion}
ghci> quickCheckWith (stdArgs {maxSize=7}) prop_secuenciaGradosInvariante
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop\_secuenciaGradosInvariante}}
\begin{code}
prop_secuenciaGradosInvariante
  :: Grafo Int -> Grafo Int -> Bool
prop_secuenciaGradosInvariante g h =
    not (isomorfos g h) ||
    secuenciaGrados g == secuenciaGrados h
\end{code}

