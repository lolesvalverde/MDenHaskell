A la hora de trabajar con conjuntos en Haskell, utilizaremos su 
representación como listas; es por ello que utilizaremos la librería
\texttt{Data.List}.

\ignora{
\begin{code}
module Conjuntos (  pertenece
                  , conjuntoVacio
                  , esVacio
                  , esUnitario
                  , esSubconjunto
                  , conjuntosIguales
                  , esSubconjuntoPropio
                  , productoCartesiano
                  , combinaciones
                  , variacionesR
                  ) where
  
import Test.QuickCheck
import Data.List
\end{code}
}

\subsection{Primeras definiciones}

\begin{definicion}
  Llamaremos \textbf{conjunto} a una colección de objetos, que llamaremos
  \textbf{elementos}, distintos entre sí, que comparten una propiedad. 
  Para que un conjunto esté bien definido debe ser posible discernir si un 
  objeto arbitrario está o no en él.
\end{definicion}

Los conjuntos pueden definirse de manera explícita, citando todos sus 
elementos entre llaves, de manera implícita, dando una o varias 
características que determinen si un objeto dado está o no en el conjunto.
Por ejemplo, los conjuntos $\{1,2,3,4\}$ y 
$\{x\in \mathbb{N} | 1 \leq x \leq 4\}$ son el mismo, definido de forma
explicita e implícita respectivamente. 

\begin{nota}
La definición implícita es necesaria cuando el conjunto en cuestión tiene
una cantidad infinita de elementos. En general, los conjuntos se notarán
con letras mayúsculas: $A,B,\dots$ y los elementos con letras minúsculas. 
\end{nota}

Si el elemento $a$ pertenece al conjunto $A$, escribiremos $a \in A$. 
En caso  contrario escribiremos $a \not \in A$.

La función \texttt{(pertenece xs x)} se verifica si \texttt{x}
pertece al conjunto \texttt{xs}. Por elemplo,

\begin{sesion}
pertenece conjuntoVacio 5  ==  False
pertenece [1..6] (-4)      ==  False
pertenece ['a'..'z'] 'c'   ==  True
\end{sesion}

\begin{code}
pertenece :: Eq a => [a] -> a -> Bool
pertenece xs x = elem x xs
\end{code}

\subsection{Conjunto vacío}

\begin{definicion}
  El conjunto que carece de elementos se denomina \textbf{conjunto vacío}
  y se denota por $\emptyset$.
\end{definicion}

La función \texttt{conjuntoVacio} devuelve el conjunto vacío y la
función \texttt{(esVacio xs)} se verifica si el conjunto
\texttt{xs} es vacío. Por ejemplo,

\begin{sesion}
esVacio [1..6]         ==  False
esVacio [6..1]         ==  True
esVacio conjuntoVacio  ==  True
\end{sesion}

\index{\texttt{conjuntoVacio}}
\index{\texttt{esVacio}}    
\begin{code}
conjuntoVacio :: [a]
conjuntoVacio = []

esVacio :: [a] -> Bool
esVacio = null
\end{code}

\subsection{Conjunto unitario}

\begin{definicion}
  Un conjunto con un único elemento se denomina \textbf{unitario}.
\end{definicion}
    
\begin{nota}
  Notemos que, si $X=\{x\}$ es un conjunto unitario, debemos distinguir 
  entre el conjunto $X$ y el elemento $x$.
\end{nota}

La función \texttt{(esUnitario xs)} se verifica si el conjunto 
\texttt{xs} es unitario.

\begin{sesion}
esUnitario [5]    ==  True
esUnitario [5,3]  ==  False
esUnitario [5,5]  ==  True
\end{sesion}

\index{\texttt{unitario}}
\begin{code}
esUnitario :: Eq a => [a] -> Bool
esUnitario xs = length (nub xs) == 1
\end{code}

\subsection{Subconjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, si todo elemento de $A$ es a su vez
  elemento de $B$ diremos que $A$ es un subconjunto de $B$ y lo notaremos
  $A \cup B$. En caso contrario se notará $A \not \cup B$.
\end{definicion}

La función \texttt{(esSubconjunto xs ys)} se verifica si \texttt{xs} es
un subconjunto de \texttt{ys}.

\begin{sesion}
esSubconjunto [3,2,4] [4,2]        ==  True
esSubconjunto conjuntoVacio [1,2]  ==  False
esSubconjunto [1,2] conjuntoVacio  ==  True
esSubconjunto [1,2,4] [4,2,1]      ==  True
esSubconjunto [5,2] [3,2,4]        ==  False
\end{sesion}

\index{\texttt{esSubconjunto}}
\begin{code}
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto xs ys = all (pertenece xs) ys
\end{code}

\subsection{Igualdad de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, diremos que son \textbf{iguales} si tienen
  los mismos elementos, es decir, si se verifica que $A \cup B$ y 
  $B \cup A$. Lo notaremos $A=B$.
\end{definicion}

La función \texttt{(conjuntosIguales xs ys)} se verifica si los conjuntos
\texttt{xs} y \texttt{ys} son iguales. Por ejemplo,

\begin{sesion}
conjuntosIguales [4,2] [3,2,4]  ==  True
conjuntosIguales [5,2] [3,2,4]  ==  False
\end{sesion}

\index{\texttt{conjuntosIguales}}
\begin{code}
conjuntosIguales :: Eq a => [a] -> [a] -> Bool
conjuntosIguales xs ys =
    esSubconjunto xs ys && esSubconjunto ys xs
\end{code}

\subsection{Subconjuntos propios}

\begin{definicion}
  Los subconjuntos de $A$ distintos del $\emptyset$ y del mismo $A$
  se denominan \textbf{subconjuntos propios} de $A$.
\end{definicion}

La función \texttt{(esSubconjuntoPropio xs ys)} se verifica si 
\texttt{ys} es un subconjunto propio de \texttt{xs}.

\begin{sesion}
esSubconjuntoPropio [3,2,4] [4,2]        ==  True
esSubconjuntoPropio conjuntoVacio [1,2]  ==  False
esSubconjuntoPropio [1,2] conjuntoVacio  ==  False
esSubconjuntoPropio [1,2,4] [4,2,1]      ==  False
esSubconjuntoPropio [5,2] [3,2,4]        ==  False
\end{sesion}

\index{\texttt{esSubconjuntoPropio}}
\begin{code}
esSubconjuntoPropio :: Eq a => [a] -> [a] -> Bool
esSubconjuntoPropio xs ys | esVacio ys = False
                          | conjuntosIguales xs ys = False
                          | otherwise = esSubconjunto xs ys
\end{code}

\subsection{Producto cartesiano}

\begin{definicion}
El 
  \href{https://en.wikipedia.org/wiki/Cartesian_product}
  {\textbf{producto cartesiano}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Cartesian_product}}
de dos conjuntos $A$ y $B$ es una operación sobre ellos que resulta en
un nuevo conjunto $A \times B$ que contiene a todos los pares ordenados
tales que la primera componente pertenece a $A$ y la segunda pertenece
a $B$; es decir, $A \times B = \{(a,b) | a \in A, b \in B \}$.
\end{definicion}

La función \texttt{(productoCartesiano xs ys)} devuelve el producto cartesiano
de xs e ys. Por ejemplo,

\begin{sesion}
ghci> productoCartesiano [3,1] [2,4,7]
[(3,2),(3,4),(3,7),(1,2),(1,4),(1,7)]
\end{sesion}

\index{\texttt{productoCartesiano}}
\begin{code}
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano xs ys =
  [(x,y) | x <- xs, y <- ys]
\end{code}

\subsection{Combinaciones}

\begin{definicion}
  Las \textbf{combinaciones} de un conjunto $S$ tomados en grupos 
  de $n$ son todos los subconjuntos de $S$ con $n$ elementos.
\end{definicion}

La función \texttt{(combinaciones n xs)} devuelve las combinaciones de los
elementos de \texttt{xs} en listas de \texttt{n} elementos. Por ejemplo,

\begin{sesion}
ghci> combinaciones 3 ['a'..'d']
["abc","abd","acd","bcd"]
ghci> combinaciones 2 [2,4..8]
[[2,4],[2,6],[2,8],[4,6],[4,8],[6,8]]
\end{sesion}

\index{\texttt{combinaciones}}
\begin{code}
combinaciones :: Integer -> [a] -> [[a]]
combinaciones 0 _          = [[]]
combinaciones _ []         = []
combinaciones k (x:xs) = 
    [x:ys | ys <- combinaciones (k-1) xs] ++ combinaciones k xs
\end{code}

\subsection{Variaciones con repetición}

\begin{definicion}
  Las \textbf{variaciones con repetición} de $m$ elementos tomados en grupos de
  $n$ es el número de diferentes $n$--tuplas de un conjunto de $m$ elementos.
\end{definicion}

La función \texttt{(variacionesR n xs)} devuelve las variaciones con con
repetición de los elementos de \texttt{xs} en listas de \texttt{n}
elementos. Por ejemplo,

\begin{sesion}
ghci> variacionesR 3 ['a','b']
["aaa","aab","aba","abb","baa","bab","bba","bbb"]
ghci> variacionesR 2 [2,4..8]
[[2,2],[2,4],[2,6],[2,8],[4,2],[4,4],[4,6],[4,8],
 [6,2],[6,4],[6,6],[6,8],[8,2],[8,4],[8,6],[8,8]]
\end{sesion}

\index{\texttt{variaciones}}
\begin{code}
variacionesR :: Int -> [a] -> [[a]]
variacionesR _ [] = [[]]
variacionesR 0 _  = [[]] 
variacionesR k us =
    [u:vs | u <- us, vs <- variacionesR (k-1) us]
\end{code}
