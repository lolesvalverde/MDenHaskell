A la hora de trabajar con conjuntos en Haskell, utilizaremos su 
representación como listas; es por ello que utilizaremos la librería
\texttt{Data.List}.

\ignora{
\begin{code}
module Conjuntos ( conjuntoVacio
                  , esVacio
                  , esUnitario
                  , esSubconjunto
                  ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
    
import Test.QuickCheck
import Data.List
\end{code}
}

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
una cantidad infinita de elementos.
\end{nota}

En general, los conjuntos se notarán con letras mayúsculas: $A,B,\dots$
y los elementos con letras minúsculas. Si el elemento $a$ pertenece al
conjunto $A$, escribiremos $a \in A$. En caso  contrario escribiremos
$a \not \in A$.

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

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, si todo elemento de $A$ es a su vez
  elemento de $B$ diremos que $A$ es un subconjunto de $B$ y lo notaremos
  $A \cup B$. En caso contrario se notará $A \not \cup B$.
\end{definicion}

La función \texttt{(esSubconjunto xs ys)} se verifica si \texttt{xs} es
un subconjunto de \texttt{ys}.

\begin{sesion}
esSubconjunto [4,2] [3,2,4]  ==  True
esSubconjunto [5,2] [3,2,4]  ==  False
\end{sesion}

\index{\texttt{esSubconjunto}}
\begin{code}
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto xs ys = all (`elem` ys) xs
\end{code}

