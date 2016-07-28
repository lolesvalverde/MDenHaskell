\ignora{
\begin{code}
module ConjuntosRelacionesYFunciones ( productoCartesiano
                                     , unitario
                                     , combinaciones  
                                     , variacionesR
                                     , esRelacion
                                     , imagenRelacion
                                     , dominio
                                     , rango
                                     , antiImagenRelacion
                                     , esFuncional
                                     , esFuncion
                                     , Funcion
                                     , funciones
                                     , imagen
                                     , esInyectiva
                                     , esSobreyectiva
                                     , esBiyectiva
                                     , biyecciones  
                                     , inversa
                                     , imagenInversa
                                     , conservaAdyacencia
                                     ) where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
    
import Test.QuickCheck
import Data.List
import DefinicionesYPropiedades 
\end{code}
}

En esta sección introduciremos algunos conceptos relacionados con conjuntos y
aplicaciones entre ellos que nos ayudarán posteriormente a definir relaciones
especiales entre grafos.

\subsection{Conjuntos}

\subsubsection{Producto cartesiano}

\begin{definicion}
El 
  \href{https://en.wikipedia.org/wiki/Cartesian_product}
  {\textbf{producto cartesiano}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Cartesian_product}}
de dos conjuntos $A$ y $B$ es una operación sobre ellos que resulta en
un nuevo conjunto $A \times B$ que contiene a todos los pares ordenados
tales que la primera componente pertenece a $A$ y la segunda pertenece
a $B$; es decir, $A \times B = \{(a,b) | a \in A, b \in B \}$.
\end{definicion}

La función \texttt{(productoCartesiano xs ys)} devuelve el producto cartesiano
de xs e ys. Por ejemplo,

\begin{sesion}
ghci> productoCartesiano [3,1] [2,4,7]
[(3,2),(3,4),(3,7),(1,2),(1,4),(1,7)]
\end{sesion}

\index{\texttt{productoCartesiano}}
\begin{code}
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano xs ys =
  [(x,y) | x <- xs, y <- ys]
\end{code}

\subsubsection{Conjunto unitario}

\begin{definicion}
  Un conjunto se dice \textbf{unitario} si sólo tiene un elemento.
\end{definicion}

La función \texttt{(unitario xs)} se verifica si el conjunto \texttt{xs} 
es unitario. Por ejemplo, 

\begin{sesion}
unitario [5]    ==  True
unitario [5,3]  ==  False
unitario [5,5]  ==  True
\end{sesion}

\index{\texttt{unitario}}
\begin{code}
unitario :: Eq a => [a] -> Bool
unitario xs = length (nub xs) == 1
\end{code}

\subsubsection{Combinaciones}

\begin{definicion}
  Las \textbf{combinaciones} de un conjunto $S$ tomados en grupos 
  de $n$ son todos los subconjuntos de $S$ con $n$ elementos.
\end{definicion}

La función \texttt{(combinaciones n xs)} devuelve las combinaciones de los
elementos de \texttt{xs} en listas de \texttt{n} elementos. Por ejemplo,

\begin{sesion}
ghci> combinaciones 3 ['a'..'d']
["abc","abd","acd","bcd"]
ghci> combinaciones 2 [2,4..8]
[[2,4],[2,6],[2,8],[4,6],[4,8],[6,8]]
\end{sesion}

\index{\texttt{combinaciones}}
\begin{code}
combinaciones :: Integer -> [a] -> [[a]]
combinaciones 0 _          = [[]]
combinaciones _ []         = []
combinaciones k (x:xs) = 
    [x:ys | ys <- combinaciones (k-1) xs] ++ combinaciones k xs
\end{code}

\subsubsection{Variaciones con repetición}

\begin{definicion}
  Las \textbf{variaciones con repetición} de $m$ elementos tomados en grupos de
  $n$ es el número de diferentes $n$--tuplas de un conjunto de $m$ elementos.
\end{definicion}

La función \texttt{(variacionesR n xs)} devuelve las variaciones con con
repetición de los elementos de \texttt{xs} en listas de \texttt{n}
elementos. Por ejemplo,

\begin{sesion}
ghci> variacionesR 3 ['a','b']
["aaa","aab","aba","abb","baa","bab","bba","bbb"]
ghci> variacionesR 2 [2,4..8]
[[2,2],[2,4],[2,6],[2,8],[4,2],[4,4],[4,6],[4,8],
 [6,2],[6,4],[6,6],[6,8],[8,2],[8,4],[8,6],[8,8]]
\end{sesion}

\index{\texttt{variaciones}}
\begin{code}
variacionesR :: Int -> [a] -> [[a]]
variacionesR _ [] = [[]]
variacionesR 0 _  = [[]] 
variacionesR k us =
    [u:vs | u <- us, vs <- variacionesR (k-1) us]
\end{code}

\subsection{Relaciones}

\subsubsection{Relación binaria}

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

\begin{sesion}
esRelacion [3,1] [2,4,7] [(3,4),(1,2)]  ==  True
esRelacion [3,1] [2,4,7] [(3,1),(1,2)]  ==  False
\end{sesion}

\index{\texttt{esRelacion}}
\begin{code}
esRelacion :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] -> Bool
esRelacion xs ys r =
  r `esSubconjunto` productoCartesiano xs ys
\end{code}

\subsubsection{Imagen por una relación}

\begin{definicion}
  Si $R$ es una relación binaria, la \textbf{imagen del elemento} $x$ en la
  relación $R$ es el conjunto de los valores correspondientes a $x$ en $R$.
\end{definicion}

La función \texttt{(imagenRelacion r x)} es la imagen de \texttt{x} 
en la relación \texttt{r}. Por ejemplo, 

\begin{sesion}
imagenRelacion [(1,3),(2,5),(1,4)] 1  ==  [3,4]
imagenRelacion [(1,3),(2,5),(1,4)] 2  ==  [5]
imagenRelacion [(1,3),(2,5),(1,4)] 3  ==  []
\end{sesion}

\index{\texttt{imagenRelacion}}
\begin{code}
imagenRelacion :: Eq a => [(a,b)] -> a -> [b]
imagenRelacion r x =
  [y | (z,y) <- r, z == x] 
\end{code}

\subsubsection{Dominio de una relación}

\begin{definicion}
  Dada una relación binaria $R$, su \textbf{dominio} es el conjunto que
  contiene a todos los valores que se toman en la relación $R$.
\end{definicion}

La función \texttt{(dominio r)} devuelve el dominio de la relación r.  Por
ejemplo,

\begin{sesion}
dominio [(3,2),(5,1),(3,4)]  ==  [3,5]
\end{sesion}

\index{\texttt{dominio}}
\begin{code}
dominio :: Eq a => [(a,b)] -> [a]
dominio r = nub (map fst r)
\end{code}

\subsubsection{Rango de una relación}

\begin{definicion}
  El \textbf{rango} de una relación binaria $R$ es el conjunto de las imágenes
  de mediante $R$.
\end{definicion}

La función \texttt{(rango r)} devuelve el rango de la relación binaria
\texttt{r}. Por ejemplo,

\begin{sesion}
rango [(3,2),(5,2),(3,4)]  ==  [2,4]
\end{sesion}

\index{\texttt{rango}}
\begin{code}
rango :: Eq b => [(a,b)] -> [b]
rango r = nub (map snd r)  
\end{code}

\subsubsection{Antiimagen por una función}

\begin{definicion}
  La \textbf{antiimagen del elemento} $y$ por una relación $r$ es el conjunto
  de los elementos cuya imagen es $y$.
\end{definicion}

La \texttt{(antiImagenRelacion r y)} es la antiimagen del elemento y en la
relación binaria \texttt{r}. Por ejemplo.

\begin{sesion}
antiImagenRelacion [(1,3),(2,3),(7,4)] 3  ==  [1,2]
\end{sesion}

\index{\texttt{antiImagenRelacion}}
\begin{code}
antiImagenRelacion :: Eq b => [(a,b)] -> b -> [a]
antiImagenRelacion r y =
  [x | (x,z) <- r, z == y]   
\end{code}

\subsubsection{Relación funcional}

\begin{definicion}
  Dada una relación binaria $R$, se dice \textbf{funcional} si todos los
  elementos de su dominio tienen una única imagen en $R$.
\end{definicion}

La función \texttt{(esFuncional r)} se verifica si la relación 
\texttt{r} es funcional. Por ejemplo,

\begin{sesion}
esFuncional [(3,2),(5,1),(7,9)]  ==  True
esFuncional [(3,2),(5,1),(3,4)]  ==  False
esFuncional [(3,2),(5,1),(3,2)]  ==  True
\end{sesion}

\index{\texttt{esFuncional}}
\begin{code}
esFuncional :: (Eq a, Eq b) => [(a,b)] -> Bool
esFuncional r =
  and [unitario (imagenRelacion r x) | x <- dominio r] 
\end{code}

\subsection{Funciones}

\subsubsection{Función}

\begin{definicion}
  Dada una relación $F$ entre $A$ y $B$, se dirá que es una \textbf{función} si
  es una relación binaria, es funcional y todos los elementos de $A$ están en
  el dominio.
\end{definicion}

La función \texttt{(esFuncion xs ys f)} se verifica si \texttt{f} es una
función de \texttt{xs} en \texttt{ys}. Por ejemplo,

\begin{sesion}
esFuncion [3,1] [2,4,7] [(1,7),(3,2)]        ==  True
esFuncion [3,1] [2,4,7] [(1,7)]              ==  False
esFuncion [3,1] [2,4,7] [(1,7),(3,2),(1,4)]  ==  False
\end{sesion}

\index{\texttt{esFuncion}}
\begin{code}
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
conjunto \texttt{xs} en \texttt{ys}. Por ejemplo,

\begin{sesion}
ghci> funciones [1,2,3] "ab"
[[(1,'a'),(2,'a'),(3,'a')],[(1,'a'),(2,'a'),(3,'b')],
 [(1,'a'),(2,'b'),(3,'a')],[(1,'a'),(2,'b'),(3,'b')],
 [(1,'b'),(2,'a'),(3,'a')],[(1,'b'),(2,'a'),(3,'b')],
 [(1,'b'),(2,'b'),(3,'a')],[(1,'b'),(2,'b'),(3,'b')]]
ghci> funciones [(1,2),(1,5)] "abc"
[[((1,2),'a'),((1,5),'a')],[((1,2),'a'),((1,5),'b')],
 [((1,2),'a'),((1,5),'c')],[((1,2),'b'),((1,5),'a')],
 [((1,2),'b'),((1,5),'b')],[((1,2),'b'),((1,5),'c')],
 [((1,2),'c'),((1,5),'a')],[((1,2),'c'),((1,5),'b')],
 [((1,2),'c'),((1,5),'c')]]
\end{sesion}

\index{\texttt{funciones}}
\begin{code}
funciones :: [a] -> [b] -> [Funcion a b]
funciones xs ys =
  [zip xs zs | zs <- variacionesR (length xs) ys]
\end{code}

\subsubsection{Imagen por una función}

\begin{definicion}
  Si $f$ es una función entre $A$ y $B$ y $x$ es un elemento
  del conjunto $A$, la \textbf{imagen del elemento} $x$ por la función
  $f$ es el valor asociado a $x$ por la función $f$.
\end{definicion}

La función \texttt{(imagen f x)} es la imagen del elemento \texttt{x} en la
función \texttt{f}. Por ejemplo,

\begin{sesion}
imagen [(1,7),(3,2)] 1  ==  7
imagen [(1,7),(3,2)] 3  ==  2
\end{sesion}

\index{\texttt{imagen}}
\begin{code}
imagen :: Eq a => Funcion a b -> a -> b
imagen f x = head (imagenRelacion f x)
\end{code}

\subsubsection{Función inyectiva}

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
inyectiva. Por ejemplo,

\begin{sesion}
esInyectiva [(1,4),(2,5),(3,6)]        ==  True
esInyectiva [(1,4),(2,5),(3,4)]        ==  False
esInyectiva [(1,4),(2,5),(3,6),(3,6)]  ==  True
\end{sesion}

\index{\texttt{esInyectiva}}
\begin{code}
esInyectiva :: (Eq a, Eq b) => Funcion a b -> Bool
esInyectiva f =
  all unitario [antiImagenRelacion f y | y <- rango f] 
\end{code}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos $A$ y $B$ es
  \href{https://en.wikipedia.org/wiki/Surjective_function}
  {\textbf{sobreyectiva}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Surjective_function}}
  si todos los elementos de $B$ son imagen de algún elmento de $A$.
\end{definicion}

La función \texttt{(esSobreyectiva xs ys f)} se verifica si la función
\texttt{f} es sobreyectiva. A la hora de definirla, estamos contando con que
\texttt{f} es una función entre \texttt{xs} y \texttt{ys}. Por ejemplo,

\begin{sesion}
ghci> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6)]
True
ghci> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]
False
ghci> esSobreyectiva [1,2,3] [4,5,6] [(1,4),(2,4),(3,6),(3,6)]
False
\end{sesion}

\index{\texttt{esSobreyectiva}}
\begin{code}
esSobreyectiva :: (Eq a,Eq b) => [a] -> [b] -> Funcion a b -> Bool
esSobreyectiva _ ys f =
  ys `esSubconjunto` rango f
\end{code}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos $A$ y $B$ es 
  \href{https://en.wikipedia.org/wiki/Bijective_function}
  {\textbf{biyectiva}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Bijective_function}}
  si cada elementos de $B$ es imagen de un único elemento de $A$.              
\end{definicion}

La función \texttt{(esBiyectiva xs ys f)} se verifica si la función
\texttt{f} es biyectiva.  Por ejemplo,

\begin{sesion}
ghci> esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,6),(3,6)]
True
ghci> esBiyectiva [1,2,3] [4,5,6] [(1,4),(2,5),(3,4)]
False
ghci> esBiyectiva [1,2,3] [4,5,6,7] [(1,4),(2,5),(3,6)]
False
\end{sesion}

\index{\texttt{esSobreyectiva}}
\begin{code}
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

En lo que sigue usaremos como \texttt{biyecciones} la segunda definición

\index{\texttt{biyecciones}}
\begin{code}
biyecciones :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones = biyecciones2
\end{code}
       
\begin{nota}
A partir de ahora trabajaremos con la función \texttt{biyecciones2} así  
que la redefiniremos como \texttt{biyecciones}}

\index{\texttt{biyecciones}}
\begin{code}
biyecciones :: (Eq a, Eq b) => [a] -> [b] -> [Funcion a b]
biyecciones = biyecciones2
\end{code}
\end{nota}
               
\begin{definicion}
  Si $f$ es una función biyectiva entre los conjuntos $A$ y $B$,
  definimos la
  \href{https://en.wikipedia.org/wiki/Inverse_function}
  {\textbf{función inversa}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Inverse_function}}
  como la función que a cada elemento de $B$ le hace corresponder
  el elemento de $A$ del que es imagen en $B$.
\end{definicion}

El valor de \texttt{(inversa f)} es la función inversa de \texttt{f}.  Por
ejemplo,

\begin{sesion}
ghci> inversa [(1,4),(2,5),(3,6)]
[(4,1),(5,2),(6,3)]
ghci> inversa [(1,4),(2,4),(3,6),(3,6)]
[(4,1),(4,2),(6,3)]
\end{sesion}

\index{\texttt{inversa}}
\begin{code}
inversa :: (Eq a, Eq b) => [(a,b)] -> [(b,a)]
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

\begin{sesion}
ghci> inversa [(1,4),(2,5),(3,6)]
[(4,1),(5,2),(6,3)]
ghci> inversa [(1,4),(2,4),(3,6),(3,6)]
[(4,1),(4,2),(6,3)]
\end{sesion}

\index{\texttt{imagenInversa}}
\begin{code}
imagenInversa :: (Eq a, Eq b) => [(a,b)] -> b -> a
imagenInversa f = imagen (inversa f)
\end{code}

\subsubsection{Conservar adyacencia}

\begin{definicion}
  Si $f$ es una función entre dos grafos $G = (V,A)$ y $G' = (V',A')$, diremos
  que \textbf{conserva la adyacencia} si $\forall u,v \in V$ se verifica
  $(u,v) \in A$ si y solamente si $(f(u),f(v)) \in A'$.
\end{definicion}

La función \texttt{(conservaAdyacencia g h f)} se verifica si la función
\texttt{f} entre los grafos \texttt{g} y \texttt{h} conserva las
adyacencias. Por ejemplo,

\begin{sesion}
ghci> let g1 = creaGrafo [1..4] [(1,2),(2,3),(3,4)]
ghci> let g2 = creaGrafo [1..4] [(1,2),(2,3),(2,4)]
ghci> let g3 = creaGrafo [4,6..10] [(4,8),(6,8),(8,10)]
ghci> conservaAdyacencia g1 g3 [(1,4),(2,6),(3,8),(4,10)]
False
ghci> conservaAdyacencia g2 g3 [(1,4),(2,8),(3,6),(4,10)]
True
\end{sesion}

\index{\texttt{conservaAdyacencia}}
\begin{code}
conservaAdyacencia :: (Ord a, Ord b) =>
                      Grafo a -> Grafo b -> Funcion a b -> Bool
conservaAdyacencia g h f =
    all (aristaEn h) gs && all (aristaEn g) hs
    where gs = [(imagen f x,imagen f y) | (x,y) <- aristas g]
          hs = concat [aux (x,y) | (x,y) <- aristas h]
          aux (a,b) = [(x,y) | x <- antiImagenRelacion f a,
                               y <- antiImagenRelacion f b]
\end{code}

\comentario{Ni la especificación ni la correspondiente definición coincide con
  la de MD.} 

