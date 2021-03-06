\ignora{
\begin{code}
module Relaciones ( esRelacion
                  , imagenRelacion
                  , dominio
                  , rango
                  , antiImagenRelacion
                  , esFuncional
                  ) where

import Conjuntos  ( esSubconjunto
                  , esUnitario
                  , listaAConjunto  
                  , productoCartesiano
                  )
                  
import Data.List  ( nub
                  )
\end{code}
}

\subsection{Relación binaria}

\begin{definicion}
  Una 
  \href{https://en.wikipedia.org/wiki/Binary_relation}
  {\textbf{relación binaria}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Binary_relation}}
  (o \textbf{correspondencia}) entre dos conjuntos $A$ y $B$ es
  un subconjunto del producto cartesiano $A \times B$.
\end{definicion}

La función \texttt{(esRelacion xs ys r)} se verifica si \texttt{r} es una
relación binaria de \texttt{xs} en \texttt{ys}.  Por ejemplo,

\index{\texttt{esRelacion}}
\begin{code}
-- | Ejemplos
-- >>> esRelacion [3,1] [2,4,7] [(1,2),(3,4)]
-- True
-- >>> esRelacion [3,1] [2,4,7] [(1,2),(3,1)]
-- False
esRelacion :: (Ord a, Ord b) => [a] -> [b] -> [(a,b)] -> Bool
esRelacion xs ys r =
  r `esSubconjunto` productoCartesiano xs ys 
\end{code}

\subsection{Imagen por una relación}

\begin{definicion}
  Si $R$ es una relación binaria, la \textbf{imagen del elemento} $x$ en la
  relación $R$ es el conjunto de los valores correspondientes a $x$ en $R$.
\end{definicion}

La función \texttt{(imagenRelacion r x)} es la imagen de \texttt{x} 
en la relación \texttt{r}.

\index{\texttt{imagenRelacion}}
\begin{code}
-- | Ejemplos
-- >>> imagenRelacion [(1,3),(2,5),(1,4)] 1
-- [3,4]
-- >>> imagenRelacion [(1,3),(2,5),(1,4)] 2
-- [5]
-- >>> imagenRelacion [(1,3),(2,5),(1,4)] 3
-- []
imagenRelacion :: (Ord a, Ord b) => [(a,b)] -> a -> [b]
imagenRelacion r x =
  nub [y | (z,y) <- r, z == x] 
\end{code}

\subsection{Dominio de una relación}

\begin{definicion}
  Dada una relación binaria $R$, su \textbf{dominio} es el conjunto que
  contiene a todos los valores que se toman en la relación $R$.
\end{definicion}

La función \texttt{(dominio r)} devuelve el dominio de la relación r.

\index{\texttt{dominio}}
\begin{code}
-- | Ejemplo
-- >>> dominio [(3,2),(5,1),(3,4)]
-- [3,5]
dominio :: Ord a => [(a,b)] -> [a]
dominio r = listaAConjunto (map fst r)
\end{code}

\subsection{Rango de una relación}

\begin{definicion}
  El \textbf{rango} de una relación binaria $R$ es el conjunto de las imágenes
  de mediante $R$.
\end{definicion}

La función \texttt{(rango r)} devuelve el rango de la relación binaria
\texttt{r}.

\index{\texttt{rango}}
\begin{code}
-- | Ejemplo
-- >>> rango [(3,2),(5,2),(3,4)]
-- [2,4]
rango :: Ord b => [(a,b)] -> [b]
rango r = listaAConjunto (map snd r)  
\end{code}

\subsection{Antiimagen por una relación}

\begin{definicion}
  La \textbf{antiimagen del elemento} $y$ por una relación $r$ es el conjunto
  de los elementos cuya imagen es $y$.
\end{definicion}

La \texttt{(antiImagenRelacion r y)} es la antiimagen del elemento y en la
relación binaria \texttt{r}.

\index{\texttt{antiImagenRelacion}}
\begin{code}
-- | Ejemplo
-- >>> antiImagenRelacion [(1,3),(2,3),(7,4)] 3
-- [1,2]
antiImagenRelacion :: (Ord a, Ord b) => [(a,b)] -> b -> [a]
antiImagenRelacion r y =
  nub [x | (x,z) <- r, z == y]   
\end{code}

\subsection{Relación funcional}

\begin{definicion}
  Dada una relación binaria $R$, se dice \textbf{funcional} si todos los
  elementos de su dominio tienen una única imagen en $R$.
\end{definicion}

La función \texttt{(esFuncional r)} se verifica si la relación 
\texttt{r} es funcional.

\index{\texttt{esFuncional}}
\begin{code}
-- | Ejemplos
-- >>> esFuncional [(3,2),(5,1),(7,9)]
-- True
-- >>> esFuncional [(3,2),(5,1),(3,4)]
-- False
-- >>> esFuncional [(3,2),(5,1),(3,2)]
-- True
esFuncional :: (Ord a, Ord b) => [(a,b)] -> Bool
esFuncional r =
  and [esUnitario (imagenRelacion r x) | x <- dominio r] 
\end{code}

\ignora{
  La validación es

  > doctest Relaciones.lhs
  Examples: 38  Tried: 38  Errors: 0  Failures: 0
}
