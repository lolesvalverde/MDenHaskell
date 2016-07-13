En el módulo \texttt{GrafoConListaDeAristas} se definen las funciones del
TAD de los grafos dando su representación como listas de aristas; es
decir, representando a un grafo como dos listas, la primera será la lista de
los vértices y la segunda la de las aristas.

\begin{nota}
  Una diferencia entre vectores y listas es que en los vectores se
  tiene en tiempo constante el valor de índice $n$ pero en las listas para
  encontrar el elemento $n$--ésimo hay que recorrerla. Los vectores tiene
  acceso constante ($O(1)$) y las listas lineal ($O(n)$).
\end{nota}

\begin{code}
module GrafoConListaDeAristas 
    ( Grafo
    , creaGrafo  -- [a] -> [(a,a)] -> Grafo a
    , vertices   -- Grafo a -> [a]
    , adyacentes -- Grafo a -> a -> [a]
    , aristaEn   -- (a,a) -> Grafo a -> Bool
    , aristas    -- Grafo a -> [(a,a)]
    ) where
\end{code}

En las definiciones del presente módulo se usarán las funciones \texttt{nub} y
\texttt{sort} de la librería \texttt{Data.List}

\ignora{
\begin{code}
import Data.List (nub, sort)
\end{code}
}

Vamos a definir un nuevo tipo de dato \texttt{(Grafo a)}, que representará 
un grafo a partir de la lista de sus vértices (donde los vértices son de tipo
\texttt{a}) y de aristas (que son pares de vértices). 

\index{\texttt{Grafo}}
\begin{code}
data Grafo a = G [a] [(a,a)]
    deriving (Eq, Show)
\end{code}

Las funciones básicas que definiremos a partir de este tipo coincidirán con las
indicadas en el TAD de los grafos.

\begin{itemize} 

\item \texttt{(creaGrafo vs as)} es el grafo cuyo conjunto de
  vértices es \texttt{cs} y el de sus aristas es \texttt{as}.

\index{\texttt{creaGrafo}} 
\begin{code}
creaGrafo :: Ord a => [a] -> [(a,a)] -> Grafo a
creaGrafo vs as =
  G (sort vs) (nub (sort [parOrdenado a | a <- as]))

parOrdenado :: Ord a => (a,a) -> (a,a)
parOrdenado (x,y) | x <= y    = (x,y)
                  | otherwise = (y,x)
\end{code}

\begin{ejemplo}
\texttt{ejGrafo} es el grafo

\begin{center}
\begin{tikzpicture}
  \GraphInit[vstyle=Shade]
  \SetVertexNoLabel
  \grWheel[RA=1.8]{5}
  \AssignVertexLabel {a}{1,2,3,4,5}
\end{tikzpicture}
\end{center}

\begin{sesion}
ghci> ejGrafo
 G [1,2,3,4,5] [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
\end{sesion}

\index{\texttt{ejGrafo}}
\begin{code}
ejGrafo :: Grafo Int
ejGrafo = creaGrafo [1..5]
                    [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
\end{code}
\end{ejemplo}

\item \texttt{(vertices g)} es la lista de los vértices del grafo \texttt{g}.
  Por ejemplo,
 
\begin{sesion}
vertices ejGrafo   ==  [1,2,3,4,5]
\end{sesion}

\index{\texttt{vertices}}
\begin{code}
vertices :: Grafo a -> [a]
vertices (G vs _) = vs
\end{code}

\item \texttt{(adyacentes g v)} es la lista de los vértices adyacentes 
  al vértice \texttt{v} en el grafo \texttt{g}. Por ejemplo,

\begin{sesion}
adyacentes ejGrafo 4  ==  [2,3,5]
adyacentes ejGrafo 2  ==  [1,4,5]
\end{sesion}

\index{\texttt{adyacentes}}
\begin{code}
adyacentes :: Eq a => Grafo a -> a -> [a]
adyacentes (G _ as) v =
  [u | (u,x) <- as, x == v] ++
  [u | (x,u) <- as, x == v] 
\end{code}

\item \texttt{(aristaEn g a)} se verifica si \texttt{a} es una arista del grafo
  \texttt{g}. Por ejemplo,

\begin{sesion}
aristaEn (5,1) ejGrafo  ==  True
aristaEn (3,1) ejGrafo  ==  False
\end{sesion}

\index{\texttt{aristaEn}}
\begin{code}
aristaEn :: Ord a => (a,a) -> Grafo a -> Bool
aristaEn a (G _ as) = elem (parOrdenado a) as
\end{code}

\item \texttt{(aristas g)} es la lista de las aristas del grafo \texttt{g}. 
  Por ejemplo,

\begin{sesion}
ghci> aristas ejGrafo
[(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
\end{sesion}

\index{\texttt{aristas}}
\begin{code}
aristas :: Grafo a -> [(a,a)]
aristas (G _ as) = as 
\end{code}
\end{itemize}
