En el módulo \texttt{ConjuntosConListas} se definen las funciones del TAD de
los conjuntos dando su representación como listas sin elementos repetidos.

\begin{code}
module ConjuntosConListas
    ( Conj
    , vacio           -- Conj a                         
    , inserta         -- Eq a => a -> Conj a -> Conj a
    , listaAConjunto  -- Eq a => Conj a -> Conj a
    , elimina         -- Eq a => a -> Conj a -> Conj a
    , pertenece       -- Eq a => Conj a -> a -> Bool  
    , esVacio         -- Conj a -> Bool
    , minimoElemento  -- Ord a => Conj a -> a
    ) where
\end{code}
\ignora{
\begin{code}
import Data.List
\end{code}
}

En las definiciones del presente módulo se usarán algunas funciones 
de la librería \texttt{Data.List}

Vamos a definir un nuevo tipo de dato \texttt{(Conj a)} , que representa
a los conjuntos como listas sin elementos repetidos.

\begin{code}
type Conj a = [a]
\end{code}

Las funciones básicas que definiremos a partir de este tipo coincidirán con las
indicadas en el TAD de los conjuntos.

\begin{itemize} 

\item \texttt{(vacio)} es el conjunto vacío.

\index{\texttt{vacio}} 
\begin{code}
-- | Ejemplo
-- >>> vacio
-- []
vacio :: Conj a                        
vacio = []
\end{code}

\item \texttt{(inserta x c)} es el conjunto obtenido añadiendo el elemento
  \texttt{x} al conjunto \texttt{c}.

\index{\texttt{inserta}}
\begin{code}
-- | Ejemplo
-- >>> inserta 5 vacio
-- [5]
-- >>> foldr inserta vacio [2,2,1,1,2,4,2]
-- [1,4,2]
inserta :: Eq a => a -> Conj a -> Conj a
inserta x [] = [x]
inserta x ys | elem x ys = ys
             | otherwise = x:ys
\end{code}

\item \texttt{(listaAConjunto xs)} devuelve el conjunto cuyos elementos son los
  de la lista \texttt{xs}.

\begin{code}
-- | Ejemplo
-- >>> listaAConjunto [2,2,1,1,2,4,2]
-- [2,1,4]
listaAConjunto :: Eq a => [a] -> Conj a
listaAConjunto = nub
\end{code}

\item \texttt{(esVacio c)} se verifica si \texttt{c} es el conjunto vacío.

\index{\texttt{esVacio}}
\begin{code}
-- | Ejemplos
-- >>> esVacio (listaAConjunto [2,5,1,3,7,5,3,2,1,9,0])
-- False
-- >>> esVacio vacio
-- True
esVacio :: Conj a -> Bool                
esVacio = null
\end{code}

\item \texttt{(pertenece x c)} se verifica si \texttt{x} es un elemento del
  conjunto \texttt{c}.

\index{\texttt{pertenece}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [2,5,1,3,7,5,3,2,1,9,0]
-- >>> pertenece c1 3 
-- True
-- >>> pertenece c1 4
-- False
pertenece :: Eq a => Conj a -> a -> Bool 
pertenece xs x = elem x xs
\end{code}

\comentario{Cambiar el orden de los argumentos de pertenece para simplificar la
  definición.} 

\item \texttt{(elimina x c)} es el conjunto obtenido eliminando el elemento
  \texttt{x} del conjunto \texttt{c}.

\index{\texttt{elimina}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [2,5,1,3,7,5,3,2,1,9,0]
-- >>> elimina 3 c1
-- [2,5,1,7,9,0]
-- >>> elimina 4 c1
-- [2,5,1,3,7,9,0]
elimina :: Eq a => a -> Conj a -> Conj a
elimina x xs = xs \\ [x]
\end{code}

\comentario{Se puede mejorar usando \texttt{delete}.}

\item \texttt{(minimoElemento c)} devuelve el mínimo elemento del conjunto
  \texttt{c}.

\index{\texttt{minimoElemento}}
\begin{code}
-- | Ejemplos
-- >>> minimoElemento (listaAConjunto [2,5,1,3,7,5,3,2,1,9,0])
-- 0
-- >>> minimoElemento (listaAConjunto (['a'..'e'] ++ ['A'..'E']))
-- 'A'
minimoElemento :: Ord a => Conj a -> a
minimoElemento = minimum
\end{code}
\end{itemize}

\ignora{
  La validación es

  > doctest ConjuntosConListas.lhs
  Examples: 14  Tried: 14  Errors: 0  Failures: 0
}
