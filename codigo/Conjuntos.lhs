\ignora{
\begin{code}
module Conjuntos (  sinRepetidos
                  , esUnitario
                  , esSubconjunto
                  , conjuntosIguales
                  , esSubconjuntoPropio
                  , complementario
                  , cardinal
                  , unionConjuntos
                  , unionGeneral
                  , interseccion
                  , productoCartesiano
                  , combinaciones
                  , variacionesR
                  ) where

import ConjuntosConListasOrdenadasSinRepeticion
-- import ConjuntosConListas

import Data.List  ( (\\)
                  , union
                  , intersect
                  , nub
                  )
\end{code}
}

\subsection{Definición de conjunto}

\begin{definicion}
  Llamaremos \textbf{conjunto} a una colección de objetos, que llamaremos
  \textbf{elementos}, distintos entre sí y que comparten una propiedad. Para
  que un conjunto esté bien definido debe ser posible discernir si un objeto
  arbitrario está o no en él.
\end{definicion}

Si el elemento $a$ pertenece al conjunto $A$, escribiremos $a \in A$. 
En caso  contrario escribiremos $a \not \in A$.

\begin{nota}
  En Haskell, para poder discernir si un objeto arbitrario pertenece a un
  conjunto se necesita que su tipo pertenezca a la clase \texttt{Eq}.
\end{nota}

\begin{nota}
  Al trabajar con la representación de conjuntos como listas en Haskell, hemos
  de cuidar que los ejemplos con los que trabajemos no tengan elementos
  repetidos. La función \texttt{(sinRepetidos xs)} se verifica si la  
  lista \texttt{xs} no tiene ningún elemento repetido.

\index{\texttt{sinRepetidos}}
\begin{code}
-- | Ejemplos
-- >>> sinRepetidos []
-- True
-- >>> sinRepetidos [1,2,3,1]
-- False
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos xs = nub xs == xs
\end{code}
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

\subsection{Subconjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, si todo elemento de $A$ es a su vez
  elemento de $B$ diremos que $A$ es un subconjunto de $B$ y lo notaremos
  $A \subseteq B$. En caso contrario se notará $A \not \subseteq B$.
\end{definicion}

La función \texttt{(esSubconjunto c1 c2)} se verifica si \texttt{c1} es
un subconjunto de \texttt{c2}.

\index{\texttt{esSubconjunto'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [4,2]
-- >>> let c2 = listaAConjunto [3,2,4]
-- >>> let c3 = listaAConjunto [4,2,1]
-- >>> let c4 = listaAConjunto [1,2,4]
-- >>> c1 `esSubconjunto'` c2
-- True
-- >>> c1 `esSubconjunto'` vacio
-- False
-- >>> vacio `esSubconjunto'` c2
-- True
-- >>> c3 `esSubconjunto'` c4
-- True
-- >>> c2 `esSubconjunto'` c1
-- False
esSubconjunto' :: Ord a => Conj a -> Conj a -> Bool
esSubconjunto' c1 c2
    | esVacio c1             = True
    | pertenece c2 (min c1)  = esSubconjunto' (elimina (min c1) c1) c2
    | otherwise              = False
      where min = minimoElemento
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, comprobar si       
un conjunto está contenido en otro es más sencilla. La función 
\texttt{(esSubconjunto c1 c2)} se verifica si \texttt{c1} es un
subconjunto de \texttt{c2}.

\index{\texttt{esSubconjunto}}
\begin{code}
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto c1 c2 = all (`elem` c2) c1
\end{code}

\subsection{Igualdad de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$, diremos que son \textbf{iguales} si tienen los
  mismos elementos; es decir, si se verifica que $A \subseteq B$ y
  $B \subseteq A$. Lo notaremos $A = B$.
\end{definicion}

La función \texttt{(conjuntosIguales' c1 c2)} se verifica si los conjuntos
\texttt{xs} y \texttt{ys} son iguales.

\index{\texttt{conjuntosIguales'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [4,2]
-- >>> let c2 = listaAConjunto [3,2,4]
-- >>> let c3 = listaAConjunto [4,2,1]
-- >>> let c4 = listaAConjunto [1,2,4]
-- >>> let c5 = listaAConjunto [4,4,4,4,4,4,2]
-- >>> conjuntosIguales' c1 c2
-- False
-- >>> conjuntosIguales' c3 c4
-- True
-- >>> conjuntosIguales' c1 c5
-- True
conjuntosIguales' :: Ord a => Conj a -> Conj a -> Bool
conjuntosIguales' c1 c2 = 
    esSubconjunto' c1 c2 && esSubconjunto' c2 c1
\end{code}

La definición equivalente solo válida para conjuntos como listas sin   
duplicados sería:

\index{\texttt{conjuntosIguales}}
\begin{code}
conjuntosIguales :: Eq a => [a] -> [a] -> Bool
conjuntosIguales c1 c2 = 
    esSubconjunto c1 c2 && esSubconjunto c2 c1
\end{code}

\subsection{Subconjuntos propios}

\begin{definicion}
  Los subconjuntos de $A$ distintos del $\emptyset$ y del mismo $A$ se
  denominan \textbf{subconjuntos propios} de $A$.
\end{definicion}

La función \texttt{(esSubconjuntoPropio c1 c2)} se verifica si \texttt{c1} es
un subconjunto propio de \texttt{c2}.

\index{\texttt{esSubconjuntoPropio'}}
\begin{code}
-- | Ejemplos
-- >>> let u  = listaAConjunto [1..9]
-- >>> let c1 = listaAConjunto [3,2,5,7]
-- >>> esSubconjuntoPropio' u u
-- False
-- >>> esSubconjuntoPropio' c1 u
-- True
esSubconjuntoPropio' :: Ord a => Conj a -> Conj a -> Bool
esSubconjuntoPropio' c1 c2
    | esVacio c1 = False
    | conjuntosIguales' c1 c2 = False
    | otherwise = esSubconjunto' c1 c2
\end{code}

La definición equivalente solo válida para conjuntos como listas sin   
duplicados sería:

\index{texttt{esSubconjuntoPropio}}
\begin{code}
esSubconjuntoPropio :: Eq a => [a] -> [a] -> Bool
esSubconjuntoPropio c1 c2
    | null c1 = False
    | conjuntosIguales c1 c2 = False
    | otherwise = esSubconjunto c1 c2
\end{code}

\subsection{Complementario de un conjunto}

\begin{definicion}
  Dado un conjunto $A$, se define el \textbf{complementario} de
  $A$, que notaremos por $\overline{A}$ como:
\begin{equation*}
  \overline{A} = \{x | x \in U, x \not \in A \}
\end{equation*}
\end{definicion}

La función \texttt{(complementario' u c)} devuelve el complementario del 
conjunto \texttt{c} y en el universo \texttt{u}.

\index{\texttt{complementario'}}
\begin{code}
-- | Ejemplos
-- >>> let u  = listaAConjunto [1..9]
-- >>> let c1 = listaAConjunto [3,2,5,7]
-- >>> let c2 = listaAConjunto [1,4,6,8,9]
-- >>> complementario' u c1
-- {1,4,6,8,9}
-- >>> complementario' u u
-- {}
-- >>> complementario' u vacio
-- {1,2,3,4,5,6,7,8,9}
-- >>> complementario' u c2
-- {2,3,5,7}
complementario' :: Ord a => Conj a -> Conj a -> Conj a
complementario' u c
    | esVacio c = u
    | otherwise =
        complementario' (elimina (min c) u) (elimina (min c) c)
        where min = minimoElemento
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, la       
definición de una función que devuelva el complementario de un           
conjunto es mucho más sencilla. La función \texttt{(complementario c u)}
devuelve el complementario de \texttt{c} en \texttt{u}.

\begin{code}
complementario :: Eq a => [a] -> [a] -> [a]
complementario = (\\) 
\end{code}

\subsection{Cardinal de un conjunto}

\begin{definicion}
  Dado un conjunto finito $A$, denominaremos \textbf{cardinal} de $A$ al número
  de elementos que tiene y lo notaremos $|A|$.
\end{definicion}

La función \texttt{(cardinal' xs)} devuelve el cardinal del conjunto
\texttt{xs}.

\index{\texttt{cardinal'}}
\begin{code}
-- | Ejemplos
-- >>> cardinal' vacio
-- 0
-- >>> cardinal' (listaAConjunto [1..10])
-- 10
-- >>> cardinal' (listaAConjunto "chocolate")
-- 7
cardinal' :: Ord a => Conj a -> Int
cardinal' c | esVacio c = 0
           | otherwise = 1 + cardinal' (elimina (min c) c)
     where min = minimoElemento
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, podemos       
definir el cardinal de un conjunto como la longitud de la lista que lo        
representa.

\index{\texttt{cardinal}}
\begin{code}
cardinal :: [a] -> Int
cardinal = length
\end{code}

\subsection{Conjunto unitario}

\begin{definicion}
  Un conjunto con un único elemento se denomina \textbf{unitario}.
\end{definicion}
   
\begin{nota}
  Notemos que, si $X=\{x\}$ es un conjunto unitario, debemos distinguir 
  entre el conjunto $X$ y el elemento $x$.
\end{nota}

La función \texttt{(esUnitario c)} se verifica si el conjunto 
\texttt{c} es unitario.

\index{\texttt{esUnitario'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto (take 10 (repeat 5))
-- >>> let c2 = listaAConjunto "valverde"
-- >>> let c3 = listaAConjunto "coco"
-- >>> esUnitario' c1
-- True
-- >>> esUnitario' c2
-- False
-- >>> esUnitario' c2
-- False
esUnitario' :: Ord a => Conj a -> Bool
esUnitario' c | esVacio c = False
              | otherwise = esVacio (elimina (min c) c)
     where min = minimoElemento
\end{code}

La definición equivalente solo válida para conjuntos como listas sin   
duplicados sería:

\index{texttt{esUnitario}}
\begin{code}
esUnitario :: [a] -> Bool
esUnitario []     = False
esUnitario (x:xs) = null xs
\end{code}

\subsection{Unión de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$ se define la \textbf{unión} de $A$ y $B$,
  notado $A \cup B$, como el conjunto formado por aquellos elementos que
  pertenecen al menos a uno de los dos conjuntos, $A$ ó $B$; es
  decir,
\begin{equation*}
  A \cup B = \{ x \;|\; x \in A \lor x \in B \}
\end{equation*}
\end{definicion}

La función \texttt{(unionConjuntos' c1 c2)} devuelve la unión de los
conjuntos \texttt{xs} y \texttt{ys}.

\index{\texttt{unionConjuntos'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [1,3..9]
-- >>> let c2 = listaAConjunto [2,4..9]
-- >>> unionConjuntos' c1 c2
-- {1,2,3,4,5,6,7,8,9}
unionConjuntos' :: Ord a => Conj a  -> Conj a -> Conj a
unionConjuntos' c1 c2
    | esVacio c1 = c2
    | esVacio c2 = c1
    | otherwise =
        unionConjuntos' (elimina (min c1) c1) (inserta (min c1) c2)
        where min = minimoElemento
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, podemos       
usar la función \texttt{union} de la librería \texttt{Data.List}. 

\index{\texttt{unionConjuntos}}
\begin{code}
unionConjuntos :: Eq a => [a] -> [a] -> [a]
unionConjuntos = union
\end{code}

\begin{definicion}
  Dada una familia de conjuntos $\{A\}_i$ con $i \in I$, se define la 
  \textbf{unión general} de los conjuntos $A_i$ notado
  $\bigcup_{i \in I} A_i$, como el conjunto formado por aquellos 
  elementos que pertenecen al menos a uno de los conjuntos de la  
  familia; es decir,
\begin{equation*}
  \bigcup_{i \in I} A_i = \{ x\; |\; x \in A_i,\; \forall i\in I\}
\end{equation*}
\end{definicion}

La función \texttt{(unionGeneral' xss)} devuelve la unión general de la
familia de conjuntos de la lista \texttt{xss}.

\index{\texttt{unionGeneral'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [1,4..15]
-- >>> let c2 = listaAConjunto [2,5..15]
-- >>> let c3 = listaAConjunto [3,6..15]
-- >>> unionGeneral' [c1,c2,c3]
-- {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}
unionGeneral' :: Ord a => [Conj a] -> Conj a
unionGeneral' = foldr unionConjuntos' vacio
\end{code}

La definición equivalente solo válida para conjuntos como listas sin   
duplicados sería:

\index{\texttt{unionGeneral}}
\begin{code}
unionGeneral :: Eq a => [[a]] -> [a]
unionGeneral = foldr unionConjuntos [] 
\end{code}

\subsection{Intersección de conjuntos}

\begin{definicion}
  Dados dos conjuntos $A$ y $B$ se define la \textbf{intersección} de $A$ y
  $B$, notado $A \cap B$, como el conjunto formado por aquellos elementos que
  pertenecen a cada uno de los dos conjuntos, $A$ y $B$, es
  decir,
\begin{equation*}
  A \cap B = \{ x\; |\; x \in A \land x \in B \}
\end{equation*}
\end{definicion}

La función \texttt{(interseccion' c1 c2)} devuelve la intersección de los
conjuntos \texttt{c1} y \texttt{c2}.

\index{\texttt{interseccion'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [1,3..20]
-- >>> let c2 = listaAConjunto [2,4..20]
-- >>> let c3 = listaAConjunto [2,4..30]
-- >>> let c4 = listaAConjunto [4,8..30]
-- >>> let c5 = listaAConjunto "noche"
-- >>> let c6 = listaAConjunto "dia"
-- >>> interseccion' c1 c2
-- {}
-- >>> interseccion' c3 c4
-- {4,8,12,16,20,24,28}
-- >>> interseccion' c5 c6
-- {}
interseccion' :: Ord a => Conj a -> Conj a -> Conj a
interseccion' c1 c2
    | esVacio c1 || esVacio c2 =  vacio
    | m1 < m2 = interseccion' (elimina m1 c1) c2
    | m1 > m2 = interseccion' c1 (elimina (m2) c2)
    | otherwise =
        inserta m1 (interseccion' (elimina m1 c1) (elimina m2 c2))
             where m1 = minimoElemento c1
                   m2 = minimoElemento c2
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, podemos       
usar la función \texttt{intersect} de la librería \texttt{Data.List}. 

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
  a $B$; es decir, 
\begin{equation*}
A \times B = \{(a,b) \;|\; a \in A,\; b \in B \}
\end{equation*}
\end{definicion}

La función \texttt{(productoCartesiano' c1 c2)} devuelve el producto cartesiano
de xs e ys.

\index{\texttt{productoCartesiano'}}
\begin{code}
-- | Ejemplos
-- >>> let c1 = listaAConjunto [3,1]
-- >>> let c2 = listaAConjunto [2,4,7]
-- >>> productoCartesiano' c1 c2
-- {(1,2),(1,4),(1,7),(3,2),(3,4),(3,7)}
-- >>> productoCartesiano' c2 c1
-- {(2,1),(2,3),(4,1),(4,3),(7,1),(7,3)}
productoCartesiano' :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
productoCartesiano' c1 c2  
    | esVacio c1 || esVacio c2 = vacio
    | otherwise =
        u (productoUnitario m1 c2) (productoCartesiano' (elimina m1 c1) c2)
    where u = unionConjuntos'
          m1 = minimoElemento c1
          m2 = minimoElemento c2

productoUnitario :: (Ord a, Ord b) => a -> Conj b -> Conj (a,b)
productoUnitario a c
    | esVacio c = vacio
    | otherwise =
        inserta (a, min c) (productoUnitario a (elimina (min c) c))
    where min = minimoElemento
\end{code}

Cuando trabajamos con conjuntos como listas sin duplicados, la       
definición de una función que devuelva el producto cartesiano de dos           
conjuntos es mucho más sencilla. La función 
\texttt{(productoCartesiano xs ys)} devuelve el producto cartesiano de
los conjuntos \texttt{xs} e \texttt{ys}.

\index{\texttt{productoCartesiano}}
\begin{code}
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano xs ys = [ (x,y) | x <- xs , y <- ys]
\end{code}

\subsection{Combinaciones}

\begin{definicion}
  Las \textbf{combinaciones} de un conjunto $S$ tomados en grupos 
  de $n$ son todos los subconjuntos de $S$ con $n$ elementos.
\end{definicion}

La función \texttt{(combinaciones n xs)} devuelve las combinaciones de los
elementos de \texttt{xs} en listas de \texttt{n} elementos.

\index{\texttt{combinaciones}}
\begin{code}
-- | Ejemplos
-- >>> combinaciones 3 ['a'..'d']
-- ["abc","abd","acd","bcd"]
-- >>> combinaciones 2 [2,4..8]
-- [[2,4],[2,6],[2,8],[4,6],[4,8],[6,8]]
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
elementos.

\index{\texttt{variaciones}}
\begin{code}
-- | Ejemplos
-- >>> variacionesR 3 ['a','b']
-- ["aaa","aab","aba","abb","baa","bab","bba","bbb"]
-- >>> variacionesR 2 [2,3,5]
-- [[2,2],[2,3],[2,5],[3,2],[3,3],[3,5],[5,2],[5,3],[5,5]]
variacionesR :: Int -> [a] -> [[a]]
variacionesR _ [] = [[]]
variacionesR 0 _  = [[]] 
variacionesR k us =
    [u:vs | u <- us, vs <- variacionesR (k-1) us]
\end{code}

\ignora{
  La validación es

  > doctest Conjuntos.lhs 
  Examples: 77  Tried: 77  Errors: 0  Failures: 0
}  
