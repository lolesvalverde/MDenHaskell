En el módulo \texttt{ConjuntosConListasOrdenadasSinRepeticion} se   
definen las funciones del TAD de los conjuntos dando su representación 
como listas ordenadas sin repetición.

\begin{code}
module ConjuntosConListasOrdenadasSinRepeticion (
    Conj
  , vacio           -- Conj a                         
  , inserta         -- Ord a => a -> Conj a -> Conj a
  , listaAConjunto  -- Ord a => [a] -> Conj a
  , elimina         -- Ord a => a -> Conj a -> Conj a
  , pertenece       -- Ord a => Conj a -> a -> Bool  
  , esVacio         -- Conj a -> Bool
  , minimoElemento  -- Ord a => Conj a -> a
  ) where
\end{code}

En las definiciones del presente módulo se usarán algunas funciones 
de la librería \texttt{Data.List}

\ignora{
\begin{code}
import Data.List
\end{code}
}

Vamos a definir un nuevo tipo de dato \texttt{(Conj a)}, que representa
a los conjuntos como listas ordenadas sin repetición. 

\index{\texttt{Grafo}}
\begin{code}
data Conj a = Cj [a]
    deriving Eq

instance (Show a) => Show (Conj a) where
    showsPrec _ (Cj s) cad = showConj s cad

showConj []     cad = showString "{}" cad
showConj (x:xs) cad = showChar '{' (shows x (showl xs cad))
     where showl []     cad = showChar '}' cad
           showl (x:xs) cad = showChar ',' (shows x (showl xs cad))
\end{code}

Las funciones básicas que definiremos a partir de este tipo coincidirán con las
indicadas en el TAD de los conjuntos.

\begin{itemize} 

\item \texttt{(vacio)} es el conjunto vacío.

\index{\texttt{vacio}} 
\begin{code}
-- | Ejemplo
-- >>> vacio
-- {}
vacio :: Conj a                         
vacio = Cj []
\end{code}

\item \texttt{(inserta x c)} es el conjunto obtenido añadiendo el 
elemento \texttt{x} al conjunto \texttt{c}.

\index{\texttt{inserta}}
\begin{code}
-- | Ejemplo
-- >>> inserta 5 vacio
-- {5}
-- >>> foldr inserta vacio [2,2,1,1,2,4,2]
-- {1,2,4}
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (agrega x s)
  where 
   agrega x []                   = [x]                
   agrega x s@(y:ys) | x > y     = y : (agrega x ys)
                     | x < y     = x : s
                     | otherwise = s
\end{code}

\item \texttt{(listaAConjunto xs)} devuelve el conjunto cuyos elementos son los
  de la lista \texttt{xs}.

\begin{code}
-- | Ejemplo
-- >>> listaAConjunto [2,2,1,1,2,4,2]
-- {1,2,4}
listaAConjunto :: Ord a => [a] -> Conj a
listaAConjunto = foldr inserta vacio
\end{code}

\item \texttt{(esVacio c)} se verifica si \texttt{c} es el conjunto
vacío.

\index{\texttt{esVacio}}
\begin{code}
-- | Ejemplos
-- >>> esVacio (listaAConjunto [2,5,1,3,7,5,3,2,1,9,0])
-- False
-- >>> esVacio vacio
-- True
esVacio :: Conj a -> Bool                
esVacio (Cj xs) = null xs
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
pertenece :: Ord a => Conj a -> a -> Bool 
pertenece (Cj ys) x = elem x (takeWhile (<= x) ys)
\end{code}

\item \texttt{(elimina x c)} es el conjunto obtenido eliminando el elemento
  \texttt{x} del conjunto \texttt{c}.

\index{\texttt{elimina}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [2,5,1,3,7,5,3,2,1,9,0]
-- >>> elimina 3 c1
-- {0,1,2,5,7,9}
-- >>> elimina 4 c1
-- {0,1,2,3,5,7,9}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (elimina x s) where 
   elimina x []                   = []
   elimina x s@(y:ys) | x > y     = y : elimina x ys
                      | x < y     = s
                      | otherwise = ys
\end{code}

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
minimoElemento (Cj (x:xs)) = x
\end{code}
\end{itemize}

\ignora{
  La validación es

  > doctest ConjuntosConListasOrdenadasSinRepeticion.lhs
  Examples: 14  Tried: 14  Errors: 0  Failures: 0
}
