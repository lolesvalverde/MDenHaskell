En el módulo \texttt{GrafoConListaDeAristas} se definen las funciones del
TAD de los grafos dando su representación como conjuntos de aristas; es
decir, representando a un grafo como dos conjuntos, la primera será la lista
ordenada de los vértices y la segunda la lista ordenada de las aristas (en
ambas listas se excluye la posibilidad de repeticiones).

\begin{nota}
  Las ventajas de usar arrays frente a usar listas es que los array  
  tienen acceso constante ($O(1)$) a sus elementos mientras que las 
  listas tienen acceso lineal ($O(n)$) y que la actualización de un 
  elemento en un array no supone espacio extra. Sin embargo, los arrays 
  son representaciones muy rígidas: cualquier modificación en su  
  estructura, como cambiar su tamaño, supone un gran coste computacional
  pues se tendría que crear de nuevo el array y, además, sus índices deben 
  pertenecer a la clase de los objetos indexables (\texttt{Ix}), luego
  perdemos mucha flexibilidad en la representación.  
\end{nota}

\begin{code}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.List ( nub
                 , sort
                 )
import Text.PrettyPrint.GenericPretty ( Generic
                                      , Out
                                      )
\end{code}
}

Vamos a definir un nuevo tipo de dato \texttt{(Grafo a)}, que representará 
un grafo a partir de la lista de sus vértices (donde los vértices son de tipo
\texttt{a}) y de aristas (que son pares de vértices). 

\index{\texttt{Grafo}}
\begin{code}
data Grafo a = G [a] [(a,a)]
  deriving (Eq, Show, Generic)

instance (Out a) => Out (Grafo a)
\end{code}

Las funciones básicas que definiremos a partir de este tipo coincidirán con las
indicadas en el TAD de los grafos.

\begin{itemize} 

\item \texttt{(creaGrafo vs as)} es el grafo cuyo conjunto de
  vértices es \texttt{cs} y el de sus aristas es \texttt{as}.

\index{\texttt{creaGrafo}} 
\begin{code}
-- | Ejemplo
-- >>> creaGrafo [1..5] [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
-- G [1,2,3,4,5] [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
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

Los ejemplos usarán el siguiente grafo

\begin{code}
ejGrafo :: Grafo Int
ejGrafo = creaGrafo [1..5]
                    [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
\end{code}
\end{ejemplo}

\item \texttt{(vertices g)} es la lista de los vértices del grafo \texttt{g}.
 
\index{\texttt{vertices}}
\begin{code}
-- | Ejemplo
-- >>> vertices ejGrafo
-- [1,2,3,4,5]
vertices :: Grafo a -> [a]
vertices (G vs _) = vs
\end{code}

\item \texttt{(adyacentes g v)} es la lista de los vértices adyacentes 
  al vértice \texttt{v} en el grafo \texttt{g}. 

\index{\texttt{adyacentes}}
\begin{code}
-- | Ejemplos
-- >>> adyacentes ejGrafo 4
-- [1,3,5]
-- >>> adyacentes ejGrafo 3
-- [2,4,5]
adyacentes :: Eq a => Grafo a -> a -> [a]
adyacentes (G _ as) v =
  [u | (u,x) <- as, x == v] ++
  [u | (x,u) <- as, x == v] 
\end{code}

\item \texttt{(aristaEn a g)} se verifica si \texttt{a} es una arista del grafo
  \texttt{g}.

\index{\texttt{aristaEn}}
\begin{code}
-- | Ejemplos
-- >>> (5,1) `aristaEn` ejGrafo
-- True
-- >>> (3,1) `aristaEn` ejGrafo
-- False
aristaEn :: Ord a => (a,a) -> Grafo a -> Bool
aristaEn a (G _ as) = parOrdenado a `elem` as
\end{code}

\item \texttt{(aristas g)} es la lista de las aristas del grafo \texttt{g}. 

\index{\texttt{aristas}}
\begin{code}
-- | Ejemplo
-- >>> aristas ejGrafo
-- [(1,2),(1,4),(1,5),(2,3),(2,5),(3,4),(3,5),(4,5)]
aristas :: Grafo a -> [(a,a)]
aristas (G _ as) = as 
\end{code}
\end{itemize}

\ignora{
  La validación es

  > doctest GrafoConListaDeAristas.lhs
  Examples: 7  Tried: 7  Errors: 0  Failures: 0
}
