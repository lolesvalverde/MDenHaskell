\ignora{
\begin{code}
module Morfismos where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import Test.QuickCheck
import Data.List
import DefinicionesYPropiedades 
\end{code}
}

Llegados a este punto, es importante resaltar que un grafo se define como una
entidad matemática abstracta; es evidente que lo importante de un grafo no son
los nombres de sus vértices ni su representación gráfica. La propiedad que
caracteriza a un grafo es la forma en que sus vértices están unidos por las
aristas.

A priori, los siguientes grafos pueden parecer distintos:

\begin{minipage}{.5\textwidth}
  \centering
  \begin{tikzpicture}
  \SetVertexNoLabel
  \tikzstyle{every node} = [draw, 
                            shape        = circle,
                            ball color   = orange,
                            minimum size = 24pt]
  \GraphInit [vstyle = Shade]
  \node (a) at (0:1.5){a};
  \node (b) at (72:1.5){b};
  \node (c) at (144:1.5){c};
  \node (d) at (216:1.5){d};
  \node (e) at (288:1.5){e};
  \Edge (a)(b)
  \Edge (a)(c)
  \Edge (a)(e)
  \Edge (b)(d)
  \Edge (c)(d)
  \Edge (d)(e)
  \end{tikzpicture}
\end{minipage}
\begin{minipage}{.5\textwidth}
  \begin{tikzpicture}
  \SetVertexNoLabel
  \tikzstyle{every node} = [draw, 
                            shape        = circle,
                            ball color   = green,
                            minimum size = 24pt]
  \GraphInit [vstyle = Shade]
  \node (d) at (0:1.5){1};
  \node (b) at (72:1.5){2};
  \node (a) at (144:1.5){3};
  \node (e) at (216:1.5){4};
  \node (c) at (288:1.5){5};
  \tikzset{EdgeStyle/.append style = {bend left}}
  \Edge (a)(b)
  \Edge (a)(c)
  \Edge (b)(d)
  \Edge (c)(d)
  \tikzset{EdgeStyle/.append style = {bend right}}
  \Edge (a)(e)
  \Edge (d)(e)
  \end{tikzpicture}
\end{minipage}

Sin embargo, ambos grafos son grafos de cinco vértices que tienen las mismas
relaciones de vecindad entre sus nodos. En esta sección estudiaremos estas
relaciones que establecen las aristas entre los vértices de un grafo y
presentaremos algoritmos que nos permitan identificar cuándo dos grafos se
pueden relacionar mediante aplicaciones entre sus vértices cumpliendo ciertas
caracterísicas.

En primer lugar, introduciremos algunos conceptos relacionados con conjuntos y
aplicaciones entre ellos que nos ayudarán posteriormente a definir relaciones
especiales entre grafos.

\subsection{Conceptos previos}

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

La función \texttt{(productoCartesiano xs ys)} devuelve el producto
cartesiano de xs e ys. Por ejemplo,

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

\begin{definicion}
  Una 
  \href{https://en.wikipedia.org/wiki/Binary_relation}
  {\textbf{relación binaria}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Binary_relation}}
  (o \textbf{correspondencia}) entre dos conjuntos $A$ y $B$ es
  un subconjunto del producto cartesiano $A \times B$.
\end{definicion}

La función \texttt{(esRelacion xs ys r)}se verifica si 
\texttt{r} es una relación binaria de \texttt{xs} en \texttt{ys}. 
Por ejemplo,

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

\begin{definicion}
  Si $x$ es un elemento del conjunto $A$ y $R$ es una relación 
  binaria entre $A$ y $B$, la \textbf{imagen del elemento} $x$ 
  en la relación $R$ es el valor o los valores correspondientes 
  a $x$ en $B$ dado o dados por la relación $R$.
\end{definicion}

La función \texttt{(imagenRelacion r x)} es la imagen de \texttt{x} 
en la relación texttt{r}. Por ejemplo, 

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

\begin{definicion}
  Dada una relación $R$ entre $A$ y $B$, su \textbf{dominio} es el 
  subconjunto de $A$ que contiene a todos los valores que se toman 
  en la relación $R$.
\end{definicion}

La función \texttt{(dominio r)} devuelve el dominio de la relación r. 
Por ejemplo,

\begin{sesion}
dominio [(3,2),(5,1),(3,4)]  ==  [3,5]
\end{sesion}

\index{\texttt{dominio}}
\begin{code}
dominio :: Eq a => [(a,b)] -> [a]
dominio r = nub (map fst r)
\end{code}

\begin{definicion}
  Dada una relación $R$ entre $A$ y $B$, se dice \textbf{funcional} 
  si todos los elementos de su dominio tienen una única imagen en $R$. 
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

\begin{definicion}
  Dada una relación $F$ entre $A$ y $B$, se dirá que es una
  \textbf{función} si es una relación binaria, es funcional y
  todos los elementos de $A$ están en el dominio. 
\end{definicion}

La función \texttt{(esFuncion xs ys f)} se verifica si \texttt{f}
es una función de \texttt{xs} en \texttt{ys}. Por ejemplo, 

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
A lo largo de la sección representaremos a las funciones como
listas de pares.
\begin{code}
type Funcion a b = [(a,b)]
\end{code}
\end{nota}

La función \texttt{(funciones xs ys)} devuelve todas las posibles funciones del
conjunto \texttt{xs} en \texttt{ys}.  Por ejemplo,

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
  where variacionesR _ [] = [[]]
        variacionesR 0 _  = [[]] 
        variacionesR k us =
          [u:vs | u <- us, vs <- variacionesR (k-1) us]
\end{code}

\comentario{La definición de \texttt{variacionesR} se debe de hacer de forma
  global con su especificación y ejemplos. En realidad, debería de hacerse en
  un capítulo sobre combinatoria (que también es parte de matemática discreta)}

\begin{definicion}
  Si $R$ es una relación binaria entre $A$ y $B$ y $x$ es un elemento
  del conjunto $A$, la \textbf{imagen del elemento} $x$ en la relación
  $R$ es el valor asociado a $x$ por la relación $R$.
\end{definicion}

La función \texttt{(imagen f x)} es la imagen del elemento \texttt{x}
en la función \texttt{f}. Por ejemplo,

\begin{sesion}
imagen [(1,7),(3,2)] 1  ==  7
imagen [(1,7),(3,2)] 3  ==  2
\end{sesion}

\index{\texttt{imagen}}
\begin{code}
imagen :: Eq a => Funcion a b -> a -> b
imagen f x = head (imagenRelacion f x)
\end{code}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos es una
    \href{https://en.wikipedia.org/wiki/Injective_function}
    {\textbf{función inyectiva}}\
    \footnote{\url{https://en.wikipedia.org/wiki/Injective_function}}
  si a elementos distintos del dominio le corresponden elementos distintos de
  la imagen; es decir, si $\forall a,b \in \text{dominio}(f)$ tales que $a \not= b$,
  $f(a) \not= f(b)$.
\end{definicion}

La función \texttt{(esInyectiva fs)} se verifica si la función
\texttt{fs} es inyectiva.

\begin{sesion}
esInyectiva [(1,4),(2,5),(3,6)]        ==  True
esInyectiva [(1,4),(2,5),(3,4)]        ==  False
esInyectiva [(1,4),(2,5),(3,6),(3,6)]  ==  True
\end{sesion}

\index{\texttt{esInyectiva}}
\begin{code}
esInyectiva :: (Eq a, Eq b) => Funcion a b -> Bool
esInyectiva f = all p (map snd f)
  where p b = unitario [u | (u,v) <- f, v == b]
\end{code}

\comentario{Usando las siguientes funciones auxiliares se obtiene una
  definición alternativa de esInyectiva.}

\begin{code}
-- (rango r) es el rango de la relación binaria r. Por ejemplo,
--    rango [(3,2),(5,2),(3,4)]  ==  [2,4]
rango :: Eq b => [(a,b)] -> [b]
rango r = nub (map snd r)

-- (antiImagenRelacion r y) es la antiimagen del elemento y en la
-- relación binaria r.
--    antiImagenRelacion [(1,3),(2,3),(7,4)] 3  ==  [1,2]
antiImagenRelacion :: Eq b => [(a,b)] -> b -> [a]
antiImagenRelacion r y =
  [x | (x,z) <- r, z == y] 
  
esInyectiva2 :: (Eq a, Eq b) => Funcion a b -> Bool
esInyectiva2 f =
  and [unitario (antiImagenRelacion f y) | y <- rango f] 
\end{code}

\comentario{Se debería de escribir un capítulo sobre ``Conjuntos, relaciones y
  funciones'' para incluir las correspondientes funciones que estamos usando
  (como \texttt{esSubconjunto}, \texttt{esRelacion}, \texttt{esInyectiva}, \dots}

\begin{definicion}
  Diremos que una función $f$ entre dos conjuntos $A$ y $B$ es una
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
esSobreyectiva _ ys fs = all (`elem` vs) ys
    where vs = map snd fs
\end{code}

\begin{definicion}
Diremos que una función $F$ entre dos conjuntos $A$ y $B$ es una
  \href{https://en.wikipedia.org/wiki/Bijective_function}
  {\textbf{función biyectiva}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Bijective_function}}
 si todos los elementos de $B$ son imagen de algún elmento de $A$.              
\end{definicion}

La función \texttt{(esSobreyectiva xs ys fs)} se verifica si la
función \texttt{fs} es sobreyectiva. A la hora de definirla, estamos
contando con que \texttt{fs} es una función entre \texttt{xs} y
\texttt{ys}.
Por ejemplo,

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

\begin{definicion}
  Si $F$ es una función sobreyectiva entre $A$ y $B$ conjuntos,
  definimos la
  \href{https://en.wikipedia.org/wiki/Inverse_function}
  {\textbf{función inversa}}\
  \footnote{\url{https://en.wikipedia.org/wiki/Inverse_function}}
  como la función que a cada elemento de $B$ le hace corresponder
  el elemento de $A$ del que es imagen en $B$.
\end{definicion}

La función \texttt{(inversa f)} la función inversa de \texttt{f}.
Por ejemplo,
\begin{sesion}

\end{sesion}

\index{\texttt{inversa}}
\begin{code}
inversa :: [(a,b)] -> [(b,a)]
inversa [] = []
inversa ((a,b):fs) = (b,a):inversa fs 
\end{code}
    
\begin{definicion}
  Si $F$ es una función entre dos grafos $G=(V,A)$ y $G'=(V',A')$,
  diremos que conserva la adyacencia si $\forall u,v\in V$
  tales que $(u,v)\in A$ entonces verifica que $(F(u),F(v)) \in A$.
\end{definicion}

La función \texttt{(conservaAdyacencia g1 g2 f)} se verifica
si la función \texttt{f} conserva las adyacencias. Por ejemplo,

\begin{sesion}
ghci> let g1 = creaGrafo [1,2,3] [(1,2),(2,3)]
ghci> let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
ghci> conservaAdyacencia g1 g2 [(1,4),(2,6),(3,5)]
True
ghci> conservaAdyacencia g1 g2 [(1,4),(2,5),(3,6)]
False
\end{sesion}

\index{\texttt{conservaAdyacencia}}
\begin{code}
conservaAdyacencia :: (Eq a,Ord b) =>
       Grafo a -> Grafo b -> Funcion a b -> Bool
conservaAdyacencia g1 g2 f =
  and [(imagen f x,imagen f y) `aristaEn` g2
      | (x,y) <- aristas g1]
\end{code}

\subsection{Morfismo}

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un \textbf{morfismo}
  entre $G$ y $G'$ es una función $\phi: V \to V'$ que conserva las
  adyacencias.
\end{definicion}

La función \texttt{(esMorfismo g h vvs)} se verifica si la
función representada por \texttt{vvs} es un morfismo entre los 
grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> let g1 = creaGrafo [1,2,3] [(1,2),(2,3)]
ghci> let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
ghci> esMorfismo g1 g2 [(1,4),(2,6),(3,5)]
True
ghci> esMorfismo g1 g2 [(1,4),(2,5),(3,6)]
False
ghci> esMorfismo g1 g2 [(1,4),(2,6),(3,5),(7,9)]
False
\end{sesion}

\begin{code}
esMorfismo :: (Eq a,Ord b) => Grafo a -> Grafo b -> Funcion a b -> Bool
esMorfismo g1 g2 f =
  esFuncion (vertices g1) (vertices g2) f &&
  conservaAdyacencia g1 g2 f
\end{code}

La función \texttt{(morfismos g h)} devuelve todos los
posibles morfismos entre los grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> morfismos (grafoCiclo 3)
                (creaGrafo [4..6] [(4,5),(4,6),(5,6)])
[[(1,4),(2,5),(3,6)],[(1,4),(2,6),(3,5)],[(1,5),(2,4),(3,6)],
 [(1,5),(2,6),(3,4)],[(1,6),(2,4),(3,5)],[(1,6),(2,5),(3,4)]]
ghci> morfismos (bipartitoCompleto 1 2) (grafoCiclo 3)
[[(1,1),(2,2),(3,2)],[(1,1),(2,2),(3,3)],[(1,1),(2,3),(3,2)],
 [(1,1),(2,3),(3,3)],[(1,2),(2,1),(3,1)],[(1,2),(2,1),(3,3)],
 [(1,2),(2,3),(3,1)],[(1,2),(2,3),(3,3)],[(1,3),(2,1),(3,1)],
 [(1,3),(2,1),(3,2)],[(1,3),(2,2),(3,1)],[(1,3),(2,2),(3,2)]]
\end{sesion}

\index{\texttt{morfismos}}
\begin{code}
morfismos :: (Eq a, Ord b) => Grafo a -> Grafo b -> [[(a,b)]]
morfismos g h =
    [xs | xs <- funciones vs1 vs2 , esMorfismo g h xs]
        where vs1 = vertices g
              vs2 = vertices h
\end{code}

\subsection{Isomorfismo}

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un
  \textbf{isomorfismo} entre $G$ y $G'$ es un morfismo biyectivo
  cuyo inverso es morfismo entre $G'$ y $G$.
\end{definicion}

La función \texttt{(esIsomorfismo g h vss)} se verifica si la
aplicación cuyo grafo es \texttt{vvs} es un isomorfismo entre los 
grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> esIsomorfismo (grafoCiclo 3)
                    (creaGrafo ['a'..'c']
                               [('a','b'),('a','c'),('b','c')])
                    [(1,'a'),(2,'b'),(3,'c')]
True
ghci> esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,1),(2,3),(3,2)]
False
ghci> esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,3),(2,2),(2,2)] 
False
\end{sesion}

\index{\texttt{esIsomorfismo}}
\begin{code}
esIsomorfismo :: (Ord a,Ord b) =>
                 Grafo a -> Grafo b -> Funcion a b -> Bool
esIsomorfismo g h vss =
    esMorfismo g h vss &&
    esBiyectiva vs1 vs2 vss &&
    esMorfismo h g (inversa vss)
        where vs1 = vertices g
              vs2 = vertices h      
\end{code}

La función \texttt{(isomorfismos g h)} devuelve todos los isomorfismos
posibles entre los grafos \texttt{g} y \texttt{h}. Por ejemplo,

\begin{sesion}
ghci> isomorfismos (bipartitoCompleto 1 2) (grafoCiclo 3)
[]
ghci> isomorfismos (bipartitoCompleto 1 2) 
                   (creaGrafo "abc" [('a','b'),('b','c')])
[[(1,'b'),(2,'a'),(3,'c')],[(1,'b'),(2,'c'),(3,'a')]]
ghci> isomorfismos (grafoCiclo 4) 
                   (creaGrafo [5..8] [(5,7),(5,8),(6,7),(6,8)])
[[(1,5),(2,7),(3,6),(4,8)],[(1,5),(2,8),(3,6),(4,7)],
 [(1,6),(2,7),(3,5),(4,8)],[(1,6),(2,8),(3,5),(4,7)],
 [(1,7),(2,5),(3,8),(4,6)],[(1,7),(2,6),(3,8),(4,5)],
 [(1,8),(2,5),(3,7),(4,6)],[(1,8),(2,6),(3,7),(4,5)]
\end{sesion}

\index{\texttt{isomorfismos}}
\begin{code}
isomorfismos :: (Ord a,Ord b) => Grafo a -> Grafo b -> [Funcion a b]
isomorfismos g h =
        [xs | xs <- funciones vs1 vs2 , esIsomorfismo g h xs]
        where vs1 = vertices g
              vs2 = vertices h
\end{code}

\begin{definicion}
Dos grafos $G$ y $H$ se dicen \textbf{isomorfos} si existe algún
isomorfismo entre ellos.
\end{definicion}

La función \texttt{isomorfos g h} se verifica si los grafos
\texttt{g} y \texttt{h} son isomorfos. Por ejemplo,

\begin{sesion}
ghci> isomorfos (grafoRueda 4) (completo 4)
True
ghci> isomorfos (grafoRueda 5) (completo 5)
False
ghci> isomorfos (grafoEstrella 2) (bipartitoCompleto 1 2)
True
ghci> isomorfos (grafoCiclo 5) (bipartitoCompleto 2 3)
False
\end{sesion}

\index{\texttt{isomorfos}}
\begin{code}
isomorfos :: (Ord a,Ord b) => Grafo a -> Grafo b -> Bool
isomorfos g = not.null.isomorfismos g 
\end{code}

\subsection{Automorfismo}

\begin{definicion}
  Dados un grafo simples $G = (V,A)$, un \textbf{automorfismo}
  de $G$ es un automorfismo de $G$ en sí mismo.
\end{definicion}

La función \texttt{(esAutomorfismo g vss)} se verifica si la
aplicación cuyo grafo es \texttt{vvs} es un isomorfismo entre los 
grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> esAutomorfismo (bipartitoCompleto 1 2) [(1,2),(2,3),(3,1)]
False
ghci> esAutomorfismo (bipartitoCompleto 1 2) [(1,1),(2,3),(3,2)]
True
ghci> esAutomorfismo (grafoCiclo 4) [(1,2),(2,3),(3,4),(4,1)]
True
\end{sesion}

\index{\texttt{esAutomorfismo}}
\begin{code}
esAutomorfismo :: Ord a => Grafo a -> Funcion a a -> Bool
esAutomorfismo g = esIsomorfismo g g 
\end{code}

La función \texttt{automorfismos g} devuelve la lista de todos los
posibles automorfismos en \texttt{g}. Por ejemplo,

\begin{sesion}
ghci> automorfismos (grafoCiclo 4)
[[(1,1),(2,2),(3,3),(4,4)],[(1,1),(2,4),(3,3),(4,2)],
 [(1,2),(2,1),(3,4),(4,3)],[(1,2),(2,3),(3,4),(4,1)],
 [(1,3),(2,2),(3,1),(4,4)],[(1,3),(2,4),(3,1),(4,2)],
 [(1,4),(2,1),(3,2),(4,3)],[(1,4),(2,3),(3,2),(4,1)]]
ghci> automorfismos (completo 4)
[[(1,1),(2,2),(3,3),(4,4)],[(1,1),(2,2),(3,4),(4,3)],
 [(1,1),(2,3),(3,2),(4,4)],[(1,1),(2,3),(3,4),(4,2)],
 [(1,1),(2,4),(3,2),(4,3)],[(1,1),(2,4),(3,3),(4,2)],
 [(1,2),(2,1),(3,3),(4,4)],[(1,2),(2,1),(3,4),(4,3)],
 [(1,2),(2,3),(3,1),(4,4)],[(1,2),(2,3),(3,4),(4,1)],
 [(1,2),(2,4),(3,1),(4,3)],[(1,2),(2,4),(3,3),(4,1)],
 [(1,3),(2,1),(3,2),(4,4)],[(1,3),(2,1),(3,4),(4,2)],
 [(1,3),(2,2),(3,1),(4,4)],[(1,3),(2,2),(3,4),(4,1)],
 [(1,3),(2,4),(3,1),(4,2)],[(1,3),(2,4),(3,2),(4,1)],
 [(1,4),(2,1),(3,2),(4,3)],[(1,4),(2,1),(3,3),(4,2)],
 [(1,4),(2,2),(3,1),(4,3)],[(1,4),(2,2),(3,3),(4,1)],
 [(1,4),(2,3),(3,1),(4,2)],[(1,4),(2,3),(3,2),(4,1)]]
ghci> automorfismos (grafoRueda 5)
[[(1,1),(2,2),(3,3),(4,4),(5,5)],[(1,1),(2,2),(3,5),(4,4),(5,3)],
 [(1,1),(2,3),(3,2),(4,5),(5,4)],[(1,1),(2,3),(3,4),(4,5),(5,2)],
 [(1,1),(2,4),(3,3),(4,2),(5,5)],[(1,1),(2,4),(3,5),(4,2),(5,3)],
 [(1,1),(2,5),(3,2),(4,3),(5,4)],[(1,1),(2,5),(3,4),(4,3),(5,2)]]
\end{sesion}

\index{\texttt{automorfismos}}
\begin{code}
automorfismos :: Ord a => Grafo a -> [Funcion a a] 
automorfismos g = isomorfismos g g 
\end{code}

\subsection{Invariantes por isomorfismos}

\begin{definicion}
  Sea $G=(V,A)$ un grafo. Un \textbf{invariante por isomorfismos} 
  de $G$ es una propiedad de $G$ que tiene el mismo valor para todos 
  los grafos que son isomorfos a él.
\end{definicion}

\begin{teorema}
  Sean $G=(V,A)$ y $G'=(V',A')$  dos grafos y $\phi:V\to V'$ un 
  isomorfismo. Entonces, se verifica que $|V(G)|=|V(G')|$; es decir,
  el orden de un grafo es un invariante por isomorfismos.
\end{teorema}

Vamos a comprobar el teorema anterior con \texttt{QuickCheck}.

\begin{sesion}

\end{sesion}

\begin{code}
prop_gradoInvariante ::
    (Ord a,Ord b) => Grafo a -> Grafo b -> Property
prop_gradoInvariante g h =
    isomorfos g h ==> orden g == orden h
\end{code}

\begin{comentario}
La propiedad no está bien definida, me da error al comprobarla
con QuickCheck. Continúo después.
\end{comentario}
