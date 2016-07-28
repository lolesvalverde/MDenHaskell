\ignora{
\begin{code}
module RelacionesHomogeneas ( esRelacionHomogenea
                            , esReflexiva
                            , esSimetrica
                            , esAntisimetrica
                            , esTransitiva
                            , esRelacionEquivalencia
                            , esRelacionOrden) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import ConjuntosRelacionesYFunciones    
    
import Test.QuickCheck
import Data.List
import DefinicionesYPropiedades 
\end{code}
}

Para elaborar la presente sección, se han consultado los
  \href{https://rodas5.us.es/file/a774213d-a15a-41df-816c-e633fb1a5876/1/01-Conjuntos.pdf}
  {\textbf{apuntes de Álgebra Básica}}\
  \footnote{\url{https://rodas5.us.es/file/a774213d-a15a-41df-816c-e633fb1a5876/1/01-Conjuntos.pdf}}
, asignatura del primer curso del Grado en Matemáticas.


\begin{definicion}
  Una relación binaria entre dos conjuntos $A$ y $B$ se dice que es 
  \textbf{homogénea} si los conjuntos son iguales, es decir, si $A=B$.
  Si el par $(x,y)\in A A$ está en la relación homogénea $R$, diremos
  que $x$ está $R-$relacionado con $y$, o relacionado con $y$ por $R$.
  Esto se notará frecuentemente $xRy$ (nótese que el orden es importante).
\end{definicion}

La función \texttt{(esRelacionHomogenea xs r)} se verifica si \texttt{r} 
es una relación binaria homogénea en el conjunto \texttt{xs}.

\begin{sesion}
esRelacionHomogenea [1..4] [(1,2),(2,4),(3,4),(4,1)] == True
esRelacionHomogenea [1..4] [(1,2),(2,5),(3,4),(4,1)] == False
esRelacionHomogenea [1..4] [(1,2),(3,4),(4,1)]       == True
\end{sesion}

\begin{code}
esRelacionHomogenea :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionHomogenea xs r = esRelacion xs xs r
\end{code}

\begin{nota}
El segundo argumento que recibe la función ha de ser una lista de pares
con ambas componentes del mismo tipo.
\end{nota}

La función \texttt{(estaRelacionado r x y)} se verifica si \texttt{x}
está relacionado con \texttt{y} en la relación homogénea \texttt{r}.
Por ejemplo,

\begin{sesion}
estaRelacionado [(1,3),(2,5),(4,6)] 2 5 == True
estaRelacionado [(1,3),(2,5),(4,6)] 2 3 == False
\end{sesion}

\begin{code}
estaRelacionado :: Eq a => [(a,a)] -> a -> a -> Bool
estaRelacionado r x y = elem (x,y) r
\end{code}
\subsection{Relaciones reflexivas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$
  es \textbf{reflexiva} cuando todos los elementos de $A$ están relacionados
  por $R$ consigo mismos, es decir, cuando $\forall x \in A$ se tiene que $xRx$.
\end{definicion}

La función \texttt{(esReflexiva xs r)} se verifica si la relación \texttt{r} 
en \texttt{xs} es reflexiva. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esReflexiva [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esReflexiva [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esReflexiva [1..n] r
True
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esReflexiva [1..n] r
False
\end{sesion}

\begin{code}
esReflexiva :: Eq a => [a] -> [(a,a)] -> Bool
esReflexiva xs r = all (`elem` r) (zip xs xs) 
\end{code}

\begin{nota}
En el conjunto \mathds{N}, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xSy \longleftrightarrow x-y$ es par,
  \item $xTy \longleftrightarrow x$ divide a $y$,
\end{itemize}
son relaciones binarias homogéneas reflexivas.
\end{nota}
    
\subsection{Relaciones Simétricas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$
  es \textbf{simétrica} cuando $\forall (x,y) \in R$ se tiene que 
  $xRy \longrightarrow yRx$ .
\end{definicion}

La función \texttt{(esSimetrica xs r)} se verifica si la relación \texttt{r}
en \texttt{xs} es simetrica. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esSimetrica [1..n] r
False
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esSimetrica [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esSimetrica [1..n] r
False
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esSimetrica [1..n] r
False
\end{sesion}

\begin{code}
esSimetrica :: Eq a => [a] -> [(a,a)] -> Bool
esSimetrica xs r = all (`elem` r) [(y,x) | (x,y) <- r] 
\end{code}

\begin{nota}
  En el conjunto \mathds{N}, la relación caracterizada por
  $xSy \longleftrightarrow x-y$ es par, es una relación binaria
  homogénea simétrica.
\end{nota}

\subsection{Relaciones antisimétricas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$
  es \textbf{antisimétrica} cuando $\forall (x,y) \in R$ se tiene que 
  $xRy e yRx \longrightarrow y=x$ .
\end{definicion}


La función \texttt{(esAntisimetrica xs r)} se verifica si la relación \texttt{r}
en \texttt{xs} es antisimétrica. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esAntisimetrica [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esAntisimetrica [1..n] r
False
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esAntisimetrica [1..n] r
True
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esAntisimetrica [1..n] r
True
\end{sesion}

\begin{code}
esAntisimetrica :: Eq a => [a] -> [(a,a)] -> Bool
esAntisimetrica xs r = 
    all p [(x,y) | (x,y) <- r, elem (y,x) r]
        where p (a,b) = a == b
\end{code}

\begin{nota}
En el conjunto \mathds{N}, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xTy \longleftrightarrow x$ divide a $y$,
  \item $xRy \longleftrightarrow x < y$,
\end{itemize}
son relaciones binarias homogéneas antisimétricas.
\end{nota}

\subsection{Relaciones transitivas}

\begin{definicion}
  Sea $R$ una relación binaria homogénea en el conjunto $A$. Diremos que $R$
  es \textbf{transitiva} cuando $\forall (x,y),(y,z) \in R$ se tiene que 
  $xRy e yRz \longrightarrow xRz$ .
\end{definicion}


La función \texttt{(esTransitiva xs r)} se verifica si la relación \texttt{r}
en \texttt{xs} es transitiva. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esTransitiva [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esTransitiva [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esTransitiva [1..n] r
True
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esTransitiva [1..n] r
True
\end{sesion}

\begin{code}
esTransitiva :: Eq a => [a] -> [(a,a)] -> Bool
esTransitiva xs r = 
    all (`elem` r) [(x,z) | (x,y) <- r, (w,z) <- r, y==w]
\end{code}

\begin{nota}
En el conjunto \mathds{N}, las relaciones caracterizadas por:
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
\texttt{r} es una relación de equivalencia en \texttt{xs}. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esRelacionEquivalencia [1..n] r
False
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esRelacionEquivalencia [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esRelacionEquivalencia [1..n] r
False
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esRelacionEquivalencia [1..n] r
False
\end{sesion}

\begin{code}
esRelacionEquivalencia :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionEquivalencia xs r =
    esRelacionHomogenea xs r &&
    esReflexiva xs r         &&
    esSimetrica xs r         &&
    esTransitiva xs r
\end{code}

\begin{nota}
  En el conjunto \mathds{N}, la relación caracterizada por
  $xSy \longleftrightarrow x-y$ es par, es una relación de equivalencia.
\end{nota}
    
\subsection{Relaciones de orden}

\begin{definicion}
  Las relaciones homogéneas que son a la vez reflexivas, antisimétricas y 
  transitivas se denominan \textbf{relaciones de orden}.
\end{definicion}

La función \texttt{(esRelacionOrden xs r)} se verifica si 
\texttt{r} es una relación de orden en \texttt{xs}. Por ejemplo,

\begin{sesion}
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x..n]]
ghci> esRelacionOrden [1..n] r
True
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], even (x-y)]         
ghci> esRelacionOrden [1..n] r
False
ghci> let n = 50
ghci> let r = [(x,y) | x <- [1..n], y <- [1..n], mod x y == 0]         
ghci> esRelacionOrden [1..n] r
True
ghci> let n= 50
ghci> let r = [(x,y) | x <- [1..n] , y <- [x+1..n]]
ghci> esRelacionOrden [1..n] r
False
\end{sesion}

\begin{code}
esRelacionOrden :: Eq a => [a] -> [(a,a)] -> Bool
esRelacionOrden xs r =
    esRelacionHomogenea xs r &&
    esReflexiva xs r         &&
    esAntisimetrica xs r     &&
    esTransitiva xs r
\end{code}

\begin{nota}
En el conjunto \mathds{N}, las relaciones caracterizadas por:
\begin{itemize}
  \item $xRy \longleftrightarrow x \leq y$,
  \item $xTy \longleftrightarrow x$ divide a $y$,
\end{itemize}
son relaciones de orden.
\end{nota}

\subsection{Clases de equivalencia}

\begin{definicion}
  Si $R$ es una relación de equivalencia en $A$, denominamos
  \textbf{clase de equivalencia} de un elemento $x \in A$ al conjunto 
  de todos los elementos de $A$ relacionados con $x$, es decir,
  $\overline{x} = R(x) = \{y \in A | xRy\}$ donde la primera notación 
  se usa si la relación con la que se está tratando se sobreentiende, 
  y la segunda si no es así.
\end{definicion}

La función \texttt{(clasesEquivalencia xs r)} devuelve las clases de la
relación de equivalencia \texttt{r} en \texttt{xs}. Por ejemplo,

\begin{sesion}
  
\end{sesion}

\begin{code}
clasesEquivalencia :: Eq a => [a] -> [(a,a)] -> [[a]]
clasesEquivalencia _ [] = []
clasesEquivalencia (x:xs) r = c: clasesEquivalencia (xs \\ c) r
    where c = x:filter (estaRelacionado r x) xs
\end{code}

\comentario{Falta terminar, sigo después de comer}
