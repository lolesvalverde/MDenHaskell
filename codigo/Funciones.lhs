\ignora{
\begin{code}
module Funciones ( esFuncion
                 , Funcion
                 , funciones
                 , imagen
                 , esInyectiva
                 , esSobreyectiva
                 , esBiyectiva
                 , biyecciones  
                 , inversa
                 , imagenInversa
                 , conservaAdyacencia
                 ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import Conjuntos
import Relaciones
    
import Test.QuickCheck
import Data.List
\end{code}
}

\comentario{Esta sección no debe de depender de grafos. Por tanto, habría que
  eliminar sus importaciones.}

\begin{definicion}
  Dada una relación $F$ entre $A$ y $B$, se dirá que es una \textbf{función} si
  es una relación binaria, es funcional y todos los elementos de $A$ están en
  el dominio.
\end{definicion}

La función \texttt{(esFuncion xs ys f)} se verifica si \texttt{f} es una
función de \texttt{xs} en \texttt{ys}. Por ejemplo,

\begin{sesion}
esFuncion [3,1] [2,4,7] [(1,7),(3,2)]        ==  True
esFuncion [3,1] [2,4,7] [(1,7)]              ==  False
esFuncion [3,1] [2,4,7] [(1,7),(3,2),(1,4)]  ==  False
\end{sesion}

\index{\texttt{esFuncion}}
\begin{code}
esFuncion :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
esFuncion xs ys f =
  esRelacion xs ys f &&
  xs `esSubconjunto` dominio f &&
  esFuncional f
\end{code}

\begin{nota}
  A lo largo de la sección representaremos a las funciones como listas de
  pares.
\begin{code}
type Funcion a b = [(a,b)]
\end{code}
\end{nota}

La función \texttt{(funciones xs ys)} devuelve todas las posibles funciones del
conjunto \texttt{xs} en \texttt{ys}. Por ejemplo,

\begin{sesion}
ghci> funciones [1,2,3] "ab"
[[(1,'a'),(2,'a'),(3,'a')],[(1,'a'),(2,'a'),(3,'b')],
 [(1,'a'),(2,'b'),(3,'a')],[(1,'a'),(2,'b'),(3,'b')],
 [(1,'b'),(2,'a'),(3,'a')],[(1,'b'),(2,'a'),(3,'b')],
 [(1,'b'),(2,'b'),(3,'a')],[(1,'b'),(2,'b'),(3,'b')]]
ghci> funciones [(1,2),(1,5)] "abc"
[[((1,2),'a'),((1,5),'a')],[((1,2),'a'),((1,5),'b')],
 [((1,2),'a'),((1,5),'c')],[((1,2),'b'),((1,5),'a')],
 [((1,2),'b'),((1,5),'b')],[((1,2),'b'),((1,5),'c')],
 [((1,2),'c'),((1,5),'a')],[((1,2),'c'),((1,5),'b')],
 [((1,2),'c'),((1,5),'c')]]
\end{sesion}

\index{\texttt{funciones}}
\begin{code}
funciones :: [a] -> [b] -> [Funcion a b]
funciones xs ys =
  [zip xs zs | zs <- variacionesR (length xs) ys]
\end{code}

\subsection{Imagen por una función}

\begin{definicion}
  Si $f$ es una función entre $A$ y $B$ y $x$ es un elemento
  del conjunto $A$, la \textbf{imagen del elemento} $x$ por la función
  $f$ es el valor asociado a $x$ por la función $f$.
\end{definicion}

La función \texttt{(imagen f x)} es la imagen del elemento \texttt{x} en la
función \texttt{f}. Por ejemplo,

\begin{sesion}
imagen [(1,7),(3,2)] 1  ==  7
imagen [(1,7),(3,2)] 3  ==  2
\end{sesion}

\index{\texttt{imagen}}
\begin{code}
imagen :: (Eq a, Eq b) => Funcion a b -> a -> b
imagen f x = head (imagenRelacion f x)
\end{code}

\subsection{Funciones inyectivas}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos es
    \href{https://en.wikipedia.org/wiki/Injective_function}
    {\textbf{inyectiva}}\
    \footnote{\url{https://en.wikipedia.org/wiki/Injective_function}}
  si a elementos distintos del dominio le corresponden elementos distintos de
  la imagen; es decir, si $\forall a,b \in \text{dominio}(f)$ tales que 
  $a \not= b$, $f(a) \not= f(b)$.
\end{definicion}

La función \texttt{(esInyectiva fs)} se verifica si la función \texttt{fs} es
inyectiva. Por ejemplo,

\begin{sesion}
esInyectiva [(1,4),(2,5),(3,6)]        ==  True
esInyectiva [(1,4),(2,5),(3,4)]        ==  False
esInyectiva [(1,4),(2,5),(3,6),(3,6)]  ==  True
\end{sesion}

\index{\texttt{esInyectiva}}
\begin{code}
esInyectiva :: (Eq a, Eq b) => Funcion a b -> Bool
esInyectiva f =
  all esUnitario [antiImagenRelacion f y | y <- rango f] 
\end{code}

\subsection{Funciones sobreyectivas}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos $A$ y $B$ es
  \href{https://en.wikipedia.org/wiki/Surjective_function}
  {\textbf{sobreyectiva}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Surjective_function}}
  si todos los elementos de $B$ son imagen de algún elemento de $A$.
\end{definicion}

La función \texttt{(esSobreyectiva xs ys f)} se verifica si la función
\texttt{f} es sobreyectiva. A la hora de definirla, estamos contando con que
\texttt{f} es una función entre \texttt{xs} y \texttt{ys}. Por ejemplo,

\begin{sesion}
esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6)]        ==  True
esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]        ==  False
esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,4),(3,6),(3,6)]  ==  False
\end{sesion}

\index{\texttt{esSobreyectiva}}
\begin{code}
esSobreyectiva :: (Eq a,Eq b) => [a] -> [b] -> Funcion a b -> Bool
esSobreyectiva _ ys f = ys `esSubconjunto` rango f 
\end{code}

\subsection{Funciones biyectivas}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos $A$ y $B$ es 
  \href{https://en.wikipedia.org/wiki/Bijective_function}
  {\textbf{biyectiva}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Bijective_function}}
  si cada elementos de $B$ es imagen de un único elemento de $A$.              
\end{definicion}

La función \texttt{(esBiyectiva xs ys f)} se verifica si la función
\texttt{f} es biyectiva.  Por ejemplo,

\begin{sesion}
esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6),(3,6)]  ==  True
esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]        ==  False
esBiyectiva [1,2,3] [4,5,6,7] [(1,4),(2,5),(3,6)]      ==  False
\end{sesion}

\index{\texttt{esSobreyectiva}}
\begin{code}
esBiyectiva :: (Eq a, Eq b) => [a] -> [b] -> Funcion a b -> Bool
esBiyectiva xs ys f =
  esInyectiva f && esSobreyectiva xs ys f
\end{code}

La funciones \texttt{biyecciones1 xs ys} y \texttt{biyecciones2 xs ys}
devuelven la lista de todas las biyecciones entre los conjuntos
\texttt{xs} y \texttt{ys}. La primera lo hace filtrando las funciones
entre los conjuntos que son biyectivas y la segunda lo hace construyendo
únicamente las funciones biyectivas entre los conjuntos, con el
consecuente ahorro computacional.

\begin{sesion}
ghci> length (biyecciones1 [1..7] ['a'..'g'])
5040
(16.75 secs, 4,146,744,104 bytes)
ghci> length (biyecciones2 [1..7] ['a'..'g'])
5040
(0.02 secs, 0 bytes)
ghci> length (biyecciones1 [1..6] ['a'..'g'])
0
(2.53 secs, 592,625,824 bytes)
ghci> length (biyecciones2 [1..6] ['a'..'g'])
0
(0.01 secs, 0 bytes)
\end{sesion}

\begin{code}
biyecciones1 :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones1 xs ys =
  filter (esBiyectiva xs ys) (funciones xs ys)

biyecciones2 :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones2 xs ys
  | length xs /= length ys = []
  | otherwise              = [zip xs zs | zs <- permutations ys]
\end{code}

\begin{nota}
En lo que sigue trabajaremos con la función \texttt{biyecciones2} así  
que la redefiniremos como \texttt{biyecciones}.

\index{\texttt{biyecciones}}
\begin{code}
biyecciones :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones = biyecciones2
\end{code}
\end{nota}
               
\subsection{Inversa de una función}

\begin{definicion}
  Si $f$ es una función biyectiva entre los conjuntos $A$ y $B$,
  definimos la
  \href{https://en.wikipedia.org/wiki/Inverse_function}
  {\textbf{función inversa}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Inverse_function}}
  como la función que a cada elemento de $B$ le hace corresponder
  el elemento de $A$ del que es imagen en $B$.
\end{definicion}

El valor de \texttt{(inversa f)} es la función inversa de \texttt{f}.  Por
ejemplo,

\begin{sesion}
ghci> inversa [(1,4),(2,5),(3,6)]
[(4,1),(5,2),(6,3)]
ghci> inversa [(1,4),(2,4),(3,6),(3,6)]
[(4,1),(4,2),(6,3)]
\end{sesion}

\index{\texttt{inversa}}
\begin{code}
inversa :: (Eq a, Eq b) => Funcion a b -> Funcion b a
inversa f = [(y,x) | (x,y) <- nub f]
\end{code}

\comentario{En la definición de inversa se puede eliminar nub, suponiendo que f
  es un conjunto.}

\begin{nota}
Para considerar la inversa de una función, esta tiene que ser
biyectiva. Luego \texttt{(inversa f)} asigna a cada elemento del
conjunto imagen (que en este caso coincide con la imagen) uno y solo
uno del conjunto de salida.
\end{nota}

La función \texttt{(imagenInversa f y)} devuelve el elemento del conjunto   
de salida de la función \texttt{f} tal que su imagen es \texttt{y}.

\begin{sesion}
imagenInversa [(1,4),(2,5),(3,6)] 5        ==  2
imagenInversa [(1,4),(2,4),(3,6),(3,6)] 6  ==  3
\end{sesion}

\index{\texttt{imagenInversa}}
\begin{code}
imagenInversa :: (Eq a, Eq b) => Funcion a b -> b -> a 
imagenInversa f = imagen (inversa f)
\end{code}

\subsection{Conservar adyacencia}

\comentario{Esta sección debe de estar en el capítulo de grafos.}

\begin{definicion}
  Si $f$ es una función entre dos grafos $G = (V,A)$ y $G' = (V',A')$, diremos
  que \textbf{conserva la adyacencia} si $\forall u,v \in V$ se verifica  
  que si $(u,v) \in A$, entonces $(f(u),f(v)) \in A'$.
\end{definicion}

La función \texttt{(conservaAdyacencia g h f)} se verifica si la función
\texttt{f} entre los grafos \texttt{g} y \texttt{h} conserva las
adyacencias. Por ejemplo,

\begin{sesion}
ghci> let g1 = creaGrafo [1..4] [(1,2),(2,3),(3,4)]
ghci> let g2 = creaGrafo [1..4] [(1,2),(2,3),(2,4)]
ghci> let g3 = creaGrafo [4,6..10] [(4,8),(6,8),(8,10)]
ghci> conservaAdyacencia g1 g3 [(1,4),(2,6),(3,8),(4,10)]
False
ghci> conservaAdyacencia g2 g3 [(1,4),(2,8),(3,6),(4,10)]
True
\end{sesion}

\index{\texttt{conservaAdyacencia}}
\begin{code}
conservaAdyacencia :: (Ord a, Ord b) =>
                      Grafo a -> Grafo b -> Funcion a b -> Bool
conservaAdyacencia g h f = all (aristaEn h) gs
  where gs = [(imagen f x,imagen f y) | (x,y) <- aristas g]
\end{code}


