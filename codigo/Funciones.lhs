\ignora{
\begin{code}
module Funciones ( esFuncion
                 , Funcion
                 , funciones
                 , imagen
                 , imagenConjunto
                 , esInyectiva
                 , esSobreyectiva
                 , esBiyectiva
                 , biyecciones  
                 , inversa
                 , imagenInversa
                 )
       where
  
import Conjuntos  ( esSubconjunto
                  , esUnitario
                  , variacionesR
                  )
import Relaciones ( antiImagenRelacion
                  , esFuncional
                  , esRelacion
                  , dominio
                  , imagenRelacion
                  , rango
                  )
import Data.List  ( nub
                  , permutations
                  )
import Text.PrettyPrint.GenericPretty (pp)
\end{code}
}

\begin{definicion}
  Dada una relación $F$ entre $A$ y $B$, se dirá que es una \textbf{función} si
  es una relación binaria, es funcional y todos los elementos de $A$ están en
  el dominio.
\end{definicion}

La función \texttt{(esFuncion xs ys f)} se verifica si \texttt{f} es una
función de \texttt{xs} en \texttt{ys}. 

\index{\texttt{esFuncion}}
\begin{code}
-- | Ejemplos
-- >>> esFuncion [3,1] [2,4,7] [(1,7),(3,2)]
-- True
-- >>> esFuncion [3,1] [2,4,7] [(1,7)]
-- False
-- >>> esFuncion [3,1] [2,4,7] [(1,7),(3,2),(1,4)]
-- False
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
conjunto \texttt{xs} en \texttt{ys}. 

\index{\texttt{funciones}}
\begin{code}
-- | Ejemplos
-- >>> pp $ funciones [1,2] [3,4]
-- [[(1, 3),(2, 3)],[(1, 3),(2, 4)],[(1, 4),(2, 3)],
--  [(1, 4),(2, 4)]]
-- >>> pp $ funciones [1,2] [3,4,5]
-- [[(1, 3),(2, 3)],[(1, 3),(2, 4)],[(1, 3),(2, 5)],
--  [(1, 4),(2, 3)],[(1, 4),(2, 4)],[(1, 4),(2, 5)],
--  [(1, 5),(2, 3)],[(1, 5),(2, 4)],[(1, 5),(2, 5)]]
-- >>> pp $ funciones [0,1,2] [3,4]
-- [[(0, 3),(1, 3),(2, 3)],[(0, 3),(1, 3),(2, 4)],
--  [(0, 3),(1, 4),(2, 3)],[(0, 3),(1, 4),(2, 4)],
--  [(0, 4),(1, 3),(2, 3)],[(0, 4),(1, 3),(2, 4)],
--  [(0, 4),(1, 4),(2, 3)],[(0, 4),(1, 4),(2, 4)]]
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
función \texttt{f}.

\index{\texttt{imagen}}
\begin{code}
-- | Ejemplos
-- >>> imagen [(1,7),(3,2)] 1
-- 7
-- >>> imagen [(1,7),(3,2)] 3
-- 2
imagen :: (Eq a, Eq b) => Funcion a b -> a -> b
imagen f x = head (imagenRelacion f x)
\end{code}

La función \texttt{(imagenConjunto f xs)} es la imagen del conjunto 
\texttt{xs} en la función \texttt{f}.

\index{\texttt{imagenConjunto}}
\begin{code}
-- | Ejemplos
-- >>> imagenConjunto [(1,7),(3,2),(4,3)] [1,4]
-- [7,3]
-- >>> imagenConjunto [(1,7),(3,2)] [3,1]
-- [2,7]
imagenConjunto :: (Eq a, Eq b) => Funcion a b -> [a] -> [b]
imagenConjunto f xs = nub (map (imagen f) xs)
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
inyectiva.

\index{\texttt{esInyectiva}}
\begin{code}
-- | Ejemplos
-- >>> esInyectiva [(1,4),(2,5),(3,6)]
-- True
-- >>> esInyectiva [(1,4),(2,5),(3,4)]
-- False
-- >>> esInyectiva [(1,4),(2,5),(3,6),(3,6)]
-- True
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
\texttt{f} es una función entre \texttt{xs} y \texttt{ys}. 

\index{\texttt{esSobreyectiva}}
\begin{code}
-- | Ejemplos
-- >>> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6)]
-- True
-- >>> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]
-- False
-- >>> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,4),(3,6),(3,6)]
-- False
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
\texttt{f} es biyectiva.

\index{\texttt{esSobreyectiva}}
\begin{code}
-- | Ejemplos
-- >>> esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6),(3,6)]
-- True
-- >>> esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]
-- False
-- >>> esBiyectiva [1,2,3] [4,5,6,7] [(1,4),(2,5),(3,6)]
-- False
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

El valor de \texttt{(inversa f)} es la función inversa de \texttt{f}.  

\index{\texttt{inversa}}
\begin{code}
-- | Ejemplos
-- >>> inversa [(1,4),(2,5),(3,6)]
-- [(4,1),(5,2),(6,3)]
-- >>> inversa [(1,'f'),(2,'m'),(3,'a')]
-- [('f',1),('m',2),('a',3)]
inversa :: (Eq a, Eq b) => Funcion a b -> Funcion b a
inversa f = [(y,x) | (x,y) <- f]
\end{code}

\begin{nota}
Para considerar la inversa de una función, esta tiene que ser
biyectiva. Luego \texttt{(inversa f)} asigna a cada elemento del
conjunto imagen (que en este caso coincide con la imagen) uno y solo
uno del conjunto de salida.
\end{nota}

La función \texttt{(imagenInversa f y)} devuelve el elemento del conjunto   
de salida de la función \texttt{f} tal que su imagen es \texttt{y}.

\index{\texttt{imagenInversa}}
\begin{code}
-- | Ejemplos
-- >>> imagenInversa [(1,4),(2,5),(3,6)] 5
-- 2
-- >>> imagenInversa [(1,'f'),(2,'m'),(3,'a')] 'a'
-- 3
imagenInversa :: (Eq a, Eq b) => Funcion a b -> b -> a 
imagenInversa f = imagen (inversa f)
\end{code}

\ignora{
  La validación es

  > doctest Funciones.lhs
  Examples: 61  Tried: 61  Errors: 0  Failures: 0
}
