A la hora de trabajar con conjuntos en Haskell, lo haremos con su
representación como listas; por ello, utilizaremos el paquete
\texttt{Data.List}.

\comentario{Comentar las distintas opciones de representación de conjuntos del
  \href{https://www.cs.us.es/~jalonso/cursos/i1m/temas/tema-17.html}
       {tema 17 de I1M}}

\comentario{Comentar la representación elegida de conjunto.}

\comentario{Decidir si usar la misma representación que en
  GrafoConListaDeAristas.lhs (listas ordenadas sin repeticiones))}
     
\comentario{Comentar la posibilidad de usar la
  \href{https://www.cs.us.es/~jalonso/cursos/i1m/doc/manual-Data.Set.html}
       {librería de conjuntos de Haskell (Data.Set)}}

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
  \textbf{elementos}, distintos entre sí y que comparten una propiedad. Para
  que un conjunto esté bien definido debe ser posible discernir si un objeto
  arbitrario está o no en él.
\end{definicion}

\comentario{Comentar la consecuencia sobre los tipos de la últma frase de la
  definición de conjunto.}

\begin{nota}
  Al trabajar con la representación de conjuntos como listas en Haskell, hemos
  de cuidar que los ejemplos con los que trabajemos no tengan elementos
  repetidos. La función \texttt{(nub xs)} de la librería \texttt{Data.List}
  elimina los elementos repetidos de una lista.
\end{nota}

Los conjuntos pueden definirse de manera explícita, citando todos sus elementos
entre llaves, de manera implícita, dando una o varias características que
determinen si un objeto dado está o no en el conjunto.  Por ejemplo, los
conjuntos $\{1,2,3,4\}$ y $\{x\in \mathbb{N} | 1 \leq x \leq 4\}$ son el mismo,
definido de forma explicita e implícita respectivamente.

\begin{nota}
  La definición implícita es necesaria cuando el conjunto en cuestión tiene una
  cantidad infinita de elementos. En general, los conjuntos se notarán con
  letras mayúsculas: $A,B,\dots$ y los elementos con letras minúsculas:
  $a,b,\dots$.
\end{nota}

Cuando trabajamos con conjuntos concretos, siempre existe un contexto donde
esos conjuntos existen. Por ejemplo, si $A = \{-1,1,2,3,4,5\}$ y
$B = \{x | x \in \mathbb{N} \text{es par} \}$ el contexto donde podemos
considerar $A$ y $B$ es el conjunto de los números enteros, $\mathbb{Z}$.  En
general, a este conjunto se le denomina \textit{conjunto universal}.  De una
forma algo más precisa, podemos dar la siguiente definición:

\begin{definicion}
  El \textbf{conjunto universal}, que notaremos por $U$, es un conjunto del que
  son subconjuntos todos los posibles conjuntos que originan el problema que
  tratamos.
\end{definicion}

\subsection{Pertenencia a un conjunto}

Si el elemento $a$ pertenece al conjunto $A$, escribiremos $a \in A$. 
En caso  contrario escribiremos $a \not \in A$.

La función \texttt{(pertenece x xs)} se verifica si \texttt{x}
pertenece al conjunto \texttt{xs}. Por elemplo,

\begin{sesion}
9   `pertenece` [1..6]  ==  False
'c' `pertenece` "Roca"  ==  True
\end{sesion}

\begin{code}
pertenece :: Eq a => a -> [a] -> Bool
pertenece = elem
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

\comentario{En la definición de esUnitario se puede eliminar nub si se supone
  que trabajamos con conjuntos.}

\subsection{Subconjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, si todo elemento de $A$ es a su vez
  elemento de $B$ diremos que $A$ es un subconjunto de $B$ y lo notaremos
  $A \subseteq B$. En caso contrario se notará $A \not \subseteq B$.
\end{definicion}

La función \texttt{(esSubconjunto xs ys)} se verifica si \texttt{xs} es
un subconjunto de \texttt{ys}.

\begin{sesion}
[4,2]         `esSubconjunto` [3,2,4]       ==  True
[1,2]         `esSubconjunto` conjuntoVacio ==  False
conjuntoVacio `esSubconjunto` [1,2]         ==  True
[4,2,1]       `esSubconjunto` [1,2,4]       ==  True
[3,2,4]       `esSubconjunto` [5,2]         ==  False
\end{sesion}

\index{\texttt{esSubconjunto}}
\begin{code}
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto xs ys = all (`pertenece` ys) xs
\end{code}

\subsection{Igualdad de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, diremos que son \textbf{iguales} si tienen los
  mismos elementos; es decir, si se verifica que $A \subseteq B$ y
  $B \subseteq A$. Lo notaremos $A = B$.
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
  Los subconjuntos de $A$ distintos del $\emptyset$ y del mismo $A$ se
  denominan \textbf{subconjuntos propios} de $A$.
\end{definicion}

La función \texttt{(esSubconjuntoPropio xs ys)} se verifica si \texttt{xs} es
un subconjunto propio de \texttt{ys}. Por ejemplo,

\begin{sesion}
[4,2]   `esSubconjuntoPropio` [3,2,4]  ==  True
[4,2,1] `esSubconjuntoPropio` [1,2,4]  ==  False
\end{sesion}

\index{\texttt{esSubconjuntoPropio}}
\begin{code}
esSubconjuntoPropio :: Eq a => [a] -> [a] -> Bool
esSubconjuntoPropio xs ys =
  xs `esSubconjunto` ys && not (conjuntosIguales xs ys) 
\end{code}

\subsection{Complementario de un conjunto}

\begin{definicion}
  Dado un conjunto $A$, se define el \textbf{complementario} de
  $A$, que notaremos por $\overline{A}$ como:\\
  $\overline{A} = \{x | x \in U, x \not \in A \}$
\end{definicion}

La función \texttt{(unionConjuntos xs ys)} devuelve la unión de los conjuntos
\texttt{xs} y \texttt{ys}. Por ejemplo,

\begin{sesion}
complementario [1..9] [3,2,5,7]  ==  [1,4,6,8,9]
\end{sesion}

\index{\texttt{complementario}}
\begin{code}
complementario :: Eq a => [a] -> [a] -> [a]
complementario = (\\)
\end{code}

\subsection{Cardinal de un conjunto}

\begin{definicion}
  Dado un conjunto finito $A$, denominaremos \textbf{cardinal} de $A$ al número
  de elementos que tiene y lo notaremos $|A|$.
\end{definicion}

La función \texttt{(cardinal xs)} devuelve el cardinal del conjunto
\texttt{xs}. Por ejemplo,

\begin{sesion}
cardinal conjuntoVacio  ==  0
cardinal [1..10]        ==  10
\end{sesion}

\index{\texttt{cardinal}}
\begin{code}
cardinal :: Eq a => [a] -> Int
cardinal = length
\end{code}

\subsection{Unión de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$ se define la \texttt{unión} de $A$ y $B$,
  notado $A \cup B$, como el conjunto formado por aquellos elementos que
  pertenecen al menos a uno de los dos conjuntos, $A$ ó $B$; es
  decir,\\
  $A \cup B = \{ x | x \in A \lor x \in B \}$
\end{definicion}

La función \texttt{(unionConjuntos xs ys)} devuelve la unión de los
conjuntos \texttt{xs} y \texttt{ys}. Por ejemplo,

\begin{sesion}
unionConjuntos [1,3..9] [2,4..9]  ==  [1,3,5,7,9,2,4,6,8]
unionConjuntos "centri" "fugado"  ==  "centrifugado"
\end{sesion}

\index{\texttt{unionConjuntos}}
\begin{code}
unionConjuntos :: Eq a => [a] -> [a] -> [a]
unionConjuntos = union 
\end{code}

\begin{nota}
  Para ahorrar en escritura, en el futuro utilizaremos la función
  \texttt{(union xs ys)} definida en el módulo \texttt{Data.List}, equivalente
  a \texttt{(unionConjuntos xs ys)}
\end{nota}

\subsection{Intersección de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$ se define la \texttt{intersección} de $A$ y
  $B$, notado $A \cap B$, como el conjunto formado por aquellos elementos que
  pertenecen a cada uno de los dos conjuntos, $A$ y $B$, es
  decir,\\
  $A \cap B = \{ x | x \in A \land x \in B \}$
\end{definicion}

La función \texttt{(interseccion xs ys)} devuelve la intersección de los
conjuntos \texttt{xs} y \texttt{ys}. Por ejemplo,

\begin{sesion}
interseccion [1,3..20] [2,4..20]  ==  []
interseccion [2,4..30] [4,8..30]  ==  [4,8,12,16,20,24,28]
interseccion "noche" "dia"        ==  ""
\end{sesion}

\index{\texttt{interseccion}}
\begin{code}
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion = intersect
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
productoCartesiano [3,1] [2,4,7]  ==  [(3,2),(3,4),(3,7),(1,2),(1,4),(1,7)]
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
combinaciones 3 ['a'..'d']  ==  ["abc","abd","acd","bcd"]
combinaciones 2 [2,4..8]    ==  [[2,4],[2,6],[2,8],[4,6],[4,8],[6,8]]
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
