\ignora{
\begin{code}
module RelacionesHomogeneas ( esRelacionHomogenea
                            , estaRelacionado
                            , esReflexiva
                            , esSimetrica
                            , esAntisimetrica
                            , esTransitiva
                            , esRelacionEquivalencia
                            , esRelacionOrden
                            , clasesEquivalencia
                            ) where

import Conjuntos  ( esSubconjunto
                  )
import Relaciones ( esRelacion
                  )
import Data.List  ( (\\)
                  )
\end{code}
}

Para elaborar la presente sección, se han consultado los
  \href{https://rodas5.us.es/file/a774213d-a15a-41df-816c-e633fb1a5876/1/01-Conjuntos.pdf}
  {\textbf{apuntes de Álgebra Básica}},\
  \footnote{\url{https://rodas5.us.es/file/a774213d-a15a-41df-816c-e633fb1a5876/1/01-Conjuntos.pdf}}
  asignatura del primer curso del Grado en Matemáticas.

\comentario{Añadirlo al fichero de bibliografía y poner la cita.}  

\begin{definicion}
  Una relación binaria entre dos conjuntos $A$ y $B$ se dice que es 
  \textbf{homogénea} si los conjuntos son iguales; es decir, si $A = B$.
  Si el par $(x,y)\in A A$ está en la relación homogénea $R$, diremos
  que $x$ está $R$--relacionado con $y$, o relacionado con $y$ por $R$.
  Esto se notará frecuentemente $xRy$ (nótese que el orden es importante).
\end{definicion}

La función \texttt{(esRelacionHomogenea xs r)} se verifica si \texttt{r} 
es una relación binaria homogénea en el conjunto \texttt{xs}.

\index{\texttt{esRelacionHomogenea}}
\begin{code}
-- | Ejemplos
-- >>> esRelacionHomogenea [1..4] [(1,2),(2,4),(3,4),(4,1)]
-- True
-- >>> esRelacionHomogenea [1..4] [(1,2),(2,5),(3,4),(4,1)]
-- False
-- >>> esRelacionHomogenea [1..4] [(1,2),(3,4),(4,1)]
-- True
esRelacionHomogenea :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionHomogenea xs = esRelacion xs xs
\end{code}

\begin{nota}
  El segundo argumento que recibe la función ha de ser una lista de pares con
  ambas componentes del mismo tipo.
\end{nota}

La función \texttt{(estaRelacionado r x y)} se verifica si \texttt{x}
está relacionado con \texttt{y} en la relación homogénea \texttt{r}.

\index{\texttt{estaRelacionado}}
\begin{code}
-- | Ejemplos
-- >>> estaRelacionado [(1,3),(2,5),(4,6)] 2 5
-- True
-- >>> estaRelacionado [(1,3),(2,5),(4,6)] 2 3
-- False
estaRelacionado :: Eq a => [(a,a)] -> a -> a -> Bool
estaRelacionado r x y = (x,y) `elem` r
\end{code}

\subsection{Relaciones reflexivas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$
  es \textbf{reflexiva} cuando todos los elementos de $A$ están relacionados
  por $R$ consigo mismos; es decir, cuando $\forall x \in A$ se tiene que $xRx$.
\end{definicion}

La función \texttt{(esReflexiva xs r)} se verifica si la relación \texttt{r} en
\texttt{xs} es reflexiva. 

\index{\texttt{esReflexiva}}
\begin{code}
-- | Ejemplos
-- >>> esReflexiva [1,2] [(1,1),(1,2),(2,2)]
-- True 
-- >>> esReflexiva [1,2] [(1,1),(1,2)]
-- False 
esReflexiva :: Eq a => [a] -> [(a,a)] -> Bool
esReflexiva xs r = zip xs xs `esSubconjunto` r 
\end{code}

\begin{nota}
En el conjunto $\mathbb{N}$, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xSy \longleftrightarrow x-y$ es par,
  \item $xTy \longleftrightarrow x$ divide a $y$,
\end{itemize}
son relaciones binarias homogéneas reflexivas.
\end{nota}
    
\subsection{Relaciones simétricas}

\begin{definicion}
  Diremos que una relación homogénea $R$ es \textbf{simétrica} cuando
  $$\forall (x,y) \in R \longrightarrow (y,x) \in R$$
\end{definicion}

La función \texttt{(esSimetrica r)} se verifica si la relación \texttt{r} es
simétrica.

\index{\texttt{esSimetrica}}
\begin{code}
-- | Ejemplos
-- >>> esSimetrica [(1,1),(1,2),(2,1)]
-- True
-- >>> esSimetrica [(1,1),(1,2),(2,2)]
-- False
esSimetrica :: Eq a => [(a,a)] -> Bool
esSimetrica r = [(y,x) | (x,y) <- r] `esSubconjunto` r 
\end{code}

\begin{nota}
  En el conjunto $\mathbb{N}$, la relación caracterizada por
  $xSy \longleftrightarrow x-y$ es par, es una relación binaria
  homogénea simétrica.
\end{nota}

\subsection{Relaciones antisimétricas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$ es
  \textbf{antisimétrica} cuando
  $$\forall (x,y) [(x,y) \in R \wedge (y,x) \in R \longrightarrow x = y$$
\end{definicion}

La función \texttt{(esAntisimetrica r)} se verifica si la relación \texttt{r}
es antisimétrica.

\index{\texttt{esAntisimetrica}}
\begin{code}
-- | Ejemplos
-- >>> esAntisimetrica [(1,2),(3,1)]
-- True
-- >>> esAntisimetrica [(1,2),(2,1)]
-- False
esAntisimetrica :: Eq a => [(a,a)] -> Bool
esAntisimetrica r = 
  and [x == y | (x,y) <- r, (y,x) `elem` r]
\end{code}

\begin{nota}
En el conjunto $\mathbb{N}$, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xTy \longleftrightarrow x$ divide a $y$,
  \item $xRy \longleftrightarrow x < y$,
\end{itemize}
son relaciones binarias homogéneas antisimétricas.
\end{nota}

\subsection{Relaciones transitivas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$ es
  \textbf{transitiva} cuando $\forall (x,y),(y,z) \in R$ se tiene que
  $xRy \; e \; yRz \longrightarrow xRz$ .
\end{definicion}

La función \texttt{(esTransitiva r)} se verifica si la relación \texttt{r} es
transitiva.

\index{\texttt{esTransitiva}}
\begin{code}
-- | Ejemplos
-- >>> esTransitiva [(1,2),(2,3),(1,3)]
-- True
-- >>> esTransitiva [(1,2),(2,3)]
-- False
esTransitiva :: Eq a => [(a,a)] -> Bool
esTransitiva r = 
  [(x,z) | (x,y) <- r, (w,z) <- r, y == w] `esSubconjunto` r 
\end{code}

\begin{nota}
En el conjunto $\mathbb{N}$, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xSy \longleftrightarrow x-y$ es par, 
  \item $xTy \longleftrightarrow x$ divide a $y$,
  \item $xRy \longleftrightarrow x < y$,
\end{itemize}
son relaciones binarias homogéneas transitivas.
\end{nota}

\subsection{Relaciones de equivalencia}

\begin{definicion}
  Las relaciones homogéneas que son a la vez reflexivas, simétricas y 
  transitivas se denominan \textbf{relaciones de equivalencia}.
\end{definicion}

La función \texttt{(esRelacionEquivalencia xs r)} se verifica si 
\texttt{r} es una relación de equivalencia en \texttt{xs}. 

\index{\texttt{esRelacionEquivalencia}}
\begin{code}
-- | Ejemplos
-- >>> esRelacionEquivalencia [1..3] [(1,1),(2,2),(3,3),(1,2),(2,1)]
-- True
-- >>> esRelacionEquivalencia [1..3] [(2,2),(3,3),(1,2),(2,1)]
-- False
-- >>> esRelacionEquivalencia [1..3] [(1,1),(2,2),(3,3),(1,2)]
-- False
esRelacionEquivalencia :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionEquivalencia xs r =
  esReflexiva xs r   &&
  esSimetrica r      &&
  esTransitiva r
\end{code}

\begin{nota}
  En el conjunto $\mathbb{N}$, la relación caracterizada por
  $xSy \longleftrightarrow x-y$ es par, es una relación de equivalencia.
\end{nota}
    
\subsection{Relaciones de orden}

\begin{definicion}
  Las relaciones homogéneas que son a la vez reflexivas, antisimétricas y
  transitivas se denominan \textbf{relaciones de orden}.
\end{definicion}

La función \texttt{(esRelacionOrden xs r)} se verifica si \texttt{r} es una
relación de orden en \texttt{xs}.

\index{\texttt{esRelacionOrden}}
\begin{code}
-- | Ejemplo
-- >>> esRelacionOrden [1..3] [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
-- True
esRelacionOrden :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionOrden xs r =
  esReflexiva xs r  &&
  esAntisimetrica r &&
  esTransitiva r
\end{code}

\begin{nota}
En el conjunto $\mathbb{N}$, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xTy \longleftrightarrow x$ divide a $y$,
\end{itemize}
son relaciones de orden.
\end{nota}

\subsection{Clases de equivalencia}

\begin{definicion}
  Si $R$ es una relación de equivalencia en $A$, denominamos \textbf{clase de
  equivalencia} de un elemento $x \in A$ al conjunto de todos los elementos
  de $A$ relacionados con $x$; es decir,
  $\overline{x} = R(x) = \{y \in A | xRy\}$ donde la primera notación se usa si
  la relación con la que se está tratando se sobreentiende, y la segunda si no
  es así.
\end{definicion}

La función \texttt{(clasesEquivalencia xs r)} devuelve las clases de la
relación de equivalencia \texttt{r} en \texttt{xs}. 

\index{\texttt{clasesEquivalencia}}
\begin{code}
-- | Ejemplo
-- >>> let r = [(x,y) | x <- [1..5], y <- [1..5], even (x-y)]
-- >>> clasesEquivalencia [1..5] r
-- [[1,3,5],[2,4]]
clasesEquivalencia :: Eq a => [a] -> [(a,a)] -> [[a]]
clasesEquivalencia _ [] = []
clasesEquivalencia [] _ = []
clasesEquivalencia (x:xs) r = (x:c) : clasesEquivalencia (xs \\ c) r
  where c = filter (estaRelacionado r x) xs
\end{code}

\ignora{
  La validación es

  > doctest RelacionesHomogeneas.lhs
  Examples: 58  Tried: 58  Errors: 0  Failures: 0
}
