A lo largo del trabajo, utilizaré la representación de conjuntos como
listas ordenadas y sin duplicados, pensando en el trabajo que realizaré
en secciones venideras.
        
En el módulo \texttt{ConjuntosConListas} se definen las funciones del   
TAD de los conjuntos dando su representación usando listas sin repetición.

\begin{code}
module ConjuntosConListas ( vacio           -- [a]                         
                          , inserta         -- Eq a => a -> [a] -> [a]
                          , elimina         -- Eq a => a -> [a] -> [a]
                          , pertenece       -- Eq a => [a] -> a -> Bool  
                          , esVacio         -- [a] -> Bool
                          , listaAconjunto  -- Eq a =>[a] -> [a]
                          , minimoElemento  -- Ord a => [a] -> a
                          ) where
\end{code}

En las definiciones del presente módulo se usarán algunas funciones 
de la librería \texttt{Data.List}

\ignora{
\begin{code}
import Data.List
\end{code}
}

Las funciones básicas que definiremos a partir de este tipo coincidirán con las
indicadas en el TAD de los conjuntos.

\begin{itemize} 

\item \texttt{(vacio)} es el conjunto vacío.

\index{\texttt{vacio}} 
\begin{code}
-- | Ejemplo
-- >>> vacio
-- []
vacio :: [a]                        
vacio = []
\end{code}

\item \texttt{(inserta x c)} es el conjunto obtenido añadiendo el 
elemento \texttt{x} al conjunto \texttt{c}.

\index{\texttt{inserta}}
\begin{code}
-- | Ejemplo
-- >>> inserta 5 vacio
-- [5]
-- >>> foldr inserta vacio [2,2,1,1,2,4,2]
-- [1,4,2]
inserta :: Eq a => a -> [a] -> [a]
inserta x [] = [x]
inserta x ys | elem x ys = ys
             | otherwise = x:ys
\end{code}

El valor de \texttt{(listaAconjunto xs)} es el conjunto cuyos elementos son los
de la lista \texttt{xs}.

\begin{code}
-- | Ejemplo
-- >>> listaAconjunto [2,2,1,1,2,4,2]
-- [2,1,4]
listaAconjunto :: Eq a => [a] -> [a]
listaAconjunto = nub
\end{code}

\item \texttt{(esVacio c)} se verifica si \texttt{c} es el conjunto
vacío.

\index{\texttt{esVacio}}
\begin{code}
-- | Ejemplos
-- >>> esVacio (listaAconjunto [2,5,1,3,7,5,3,2,1,9,0])
-- False
-- >>> esVacio vacio
-- True
esVacio :: [a] -> Bool                
esVacio = null
\end{code}

\item \texttt{(pertenece x c)} se verifica si \texttt{x} es un elemento del
  conjunto \texttt{c}.

\index{\texttt{pertenece}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAconjunto [2,5,1,3,7,5,3,2,1,9,0]
-- >>> pertenece c1 3 
-- True
-- >>> pertenece c1 4
-- False
pertenece :: Eq a => [a] -> a -> Bool 
pertenece xs x = elem x xs
\end{code}

\item \texttt{(elimina x c)} es el conjunto obtenido eliminando el elemento
  \texttt{x} del conjunto \texttt{c}.

\index{\texttt{elimina}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAconjunto [2,5,1,3,7,5,3,2,1,9,0]
-- >>> elimina 3 c1
-- [2,5,1,7,9,0]
-- >>> elimina 4 c1
-- [2,5,1,3,7,9,0]
elimina :: Eq a => a -> [a] -> [a]
elimina x xs = xs \\ [x]
\end{code}

\item \texttt{(minimoElemento c)} devuelve el mínimo elemento del conjunto
  \texttt{c}.

\index{\texttt{minimoElemento}}
\begin{code}
-- | Ejemplos
-- >>> minimoElemento (listaAconjunto [2,5,1,3,7,5,3,2,1,9,0])
-- 0
-- >>> minimoElemento (listaAconjunto (['a'..'e'] ++ ['A'..'E']))
-- 'A'
minimoElemento :: Ord a => [a] -> a
minimoElemento = minimum
\end{code}
\end{itemize}

\ignora{
  La validación es

  > doctest ConjuntosConListas.lhs
  Examples: 14  Tried: 14  Errors: 0  Failures: 0
}
