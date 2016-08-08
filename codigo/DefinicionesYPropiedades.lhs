Una vez construida una pequeña fuente de ejemplos, estamos en condiciones de
implementar las definiciones sobre grafos en Haskell y ver que funcionan
correctamente. Además, comprobaremos que se cumplen las propiedades básicas
que se han presentado en el tema \textit{Introducción a la teoría de grafos}
de 
\href{https://dl.dropboxusercontent.com/u/15420416/tiddly/emptyMD1314.html}
     {Matemática discreta}\
\footnote{\url{https://dl.dropboxusercontent.com/u/15420416/tiddly/emptyMD1314.html}}
(\cite{Cardenas-15a}).

\begin{nota}
  Se utilizará el tipo abstracto de grafos presentados en la sección
  \ref{sec:TAD_grafos} y se utilizarán las librerias \texttt{Data.List} y
  \texttt{Test.QuickCheck}.
\end{nota}

\ignora{
\begin{code}
module DefinicionesYPropiedades (orden
                                , tamaño
                                , sonIncidentes
                                , esLazo
                                , entorno
                                , grado 
                                , esAislado
                                , esRegular
                                , valenciaMin
                                , valenciaMax
                                , esSimple
                                , secuenciaGrados
                                , secuenciaGrafica
                                , esSubgrafo
                                , esSubgrafoMax
                                , esSubgrafoPropio
                                , prop_LemaApretonDeManos
                                , prop_HavelHakimi
                                , eliminaArista
                                , eliminaVertice
                                , sumaArista
                                , sumaVertice
                                , prop_completos
                                , sumaGrafos
                                , unionGrafos
                                , grafoComplementario
                                ) where

import Data.List                      ( (\\)
                                      , sortBy
                                      , union
                                      )
import Test.QuickCheck                ( Property
                                      , (==>)
                                      , quickCheck
                                      )
import Text.PrettyPrint.GenericPretty ( pp
                                      )
import Conjuntos                      ( esSubconjunto
                                      , esVacio
                                      , conjuntosIguales
                                      )
import GrafoConListaDeAristas         ( Grafo
                                      , adyacentes
                                      , aristas
                                      , aristaEn
                                      , creaGrafo
                                      , vertices
                                      )
import EjemplosGrafos                 ( bipartitoCompleto
                                      , completo
                                      , grafoCiclo
                                      , grafoEstrella
                                      , grafoRueda
                                      , grafoPetersen
                                      )
import GeneradorGrafos 
\end{code}
}

\subsection{Definiciones de grafos}

\begin{definicion}
  El \textbf{orden} de un grafo $G=(V,A)$ se define como su número de 
  vértices. Lo denotaremos por $|V(G)|$.
\end{definicion}

La función \texttt{(orden g)} devuelve el orden del grafo \texttt{g}.

\index{\texttt{orden}}
\begin{code}
-- | Ejemplos
-- >>> orden (grafoCiclo 4)
-- 4
-- >>> orden (grafoEstrella 4)
-- 5
orden :: Grafo a -> Int
orden = length . vertices 
\end{code}

\begin{definicion}
  El \textbf{tamaño} de un grafo $G = (V,A)$ se define como su número de 
  aristas. Lo denotaremos por $|A(G)|$.
\end{definicion}

La función \texttt{(tamaño g)} devuelve el orden del grafo \texttt{g}.

\index{\texttt{tamaño}}
\begin{code}
-- | Ejempolos
-- >>> tamaño (grafoCiclo 4)
-- 4
-- >>> tamaño grafoPetersen
-- 15
tamaño :: Grafo a -> Int
tamaño = length . aristas 
\end{code}

\begin{definicion}
  Diremos que dos aristas $a, a'$ son \textbf{incidentes} si tienen
  intersección no vacía; es decir, si tienen algún vértice en común.
\end{definicion}

La función \texttt{(sonIncidentes a a')} se verifica si las aristas 
\texttt{a} y \texttt{a'} son incidentes. 
\index{\texttt{sonIncidentes}}
\begin{code}
-- | Ejemplos
-- >>> sonIncidentes (1,2) (2,4)
-- True
-- >>> sonIncidentes (1,2) (3,4)
-- False
sonIncidentes :: Eq a => (a,a) -> (a,a) -> Bool
sonIncidentes (u1,u2) (v1,v2) =
  or [u1 == v1, u1 == v2, u2 == v1, u2 == v2]
\end{code}

\begin{definicion}
  Diremos que una arista de un grafo $G$ es un \textbf{lazo} si va de un
  vértice en sí mismo. 
\end{definicion}

La función \texttt{(esLazo a)} se verifica si la arista \texttt{a} es un
lazo. 

\index{\texttt{esLazo}}
\begin{code}
-- | Ejemplos
-- >>> esLazo (4,4)
-- True
-- >>> esLazo (1,2)
-- False
esLazo :: Eq a => (a,a) -> Bool
esLazo (u,v) = u == v
\end{code}

\begin{definicion}
  Dado un grafo $G = (V,A)$, fijado un vértice $v \in V$, al conjunto de
  vértices que son adyacentes a $v$ lo llamaremos \textbf{entorno} de  
  $v$ y lo denotaremos por $N(v) = \{u \in V | (u,v) \in A\}$. 
\end{definicion}

La función \texttt{(entorno g v)} devuelve el entorno del vértice \texttt{v} en
el grafo \texttt{g}. 

\index{\texttt{entorno}}
\begin{code}
-- | Ejemplo
-- >>> entorno (creaGrafo [1..5] [(1,2),(1,3),(2,3)]) 2
-- [1,3]
entorno :: Eq a => Grafo a -> a -> [a]
entorno = adyacentes
\end{code}

\begin{definicion}
  Sea $G = (V,A)$ un grafo. El \textbf{grado} (o \textbf{valencia}) de
  $v \in V$ es $grad(v) = |N(v)|$.
\end{definicion}

La función \texttt{(grado g v)} devuelve el grado del vértice \texttt{v} 
en el grafo \texttt{g}.


\index{\texttt{grado}}
\begin{code}
-- | Ejemplos
-- >>> grado (creaGrafo [1..5] [(1,2),(1,3),(2,3)]) 2
-- 2
-- >>> grado (grafoEstrella 5) 1
-- 5
grado :: Eq a => Grafo a -> a -> Int
grado g v = length (entorno g v)
\end{code}

\begin{definicion}
  Un vértice $v$ de un grafo es \textbf{aislado} si su grado es 0.
\end{definicion}

La función \texttt{(esAislado g v)} se verifica si el vértice \texttt{v} 
es aislado en el grafo \texttt{g}. 

\index{\texttt{esAislado}}
\begin{code}
-- | Ejemplos
-- >>> esAislado (creaGrafo [1..5] [(1,2),(1,3),(2,3)]) 4
-- True
-- >>> esAislado (creaGrafo [1..5] [(1,2),(1,3),(2,3)]) 3
-- False
esAislado :: Eq a => Grafo a -> a -> Bool
esAislado g v = grado g v == 0
\end{code}

\begin{definicion}
  Un grafo es \textbf{regular} si todos sus vértices tienen el mismo grado.
\end{definicion}

La función \texttt{(esRegular g)} se verifica si el grafo \texttt{g} es regular.

\index{\texttt{esRegular}}
\begin{code}
-- | Ejemplos
-- >>> esRegular (creaGrafo [1..3] [(1,2),(1,3),(2,3)]) 
-- True
-- >>> esRegular (creaGrafo [1..4] [(1,2),(1,3),(2,4)]) 
-- False
esRegular :: Eq a => Grafo a -> Bool
esRegular g = all (==x) xs
  where (x:xs) = [grado g v | v <- vertices g]
\end{code}

\begin{definicion}
  Dado un grafo $G = (V,A)$ llamamos \textbf{valencia mínima} o \textbf{grado
  mínimo} de $G$ al valor $\delta(G) = \min \{grad(v) | v \in V\}$
\end{definicion}

La función \texttt{(valenciaMin g)} devuelve la valencia mínima del grafo
\texttt{g}.

\index{\texttt{valenciaMin}}
\begin{code}
-- | Ejemplo
-- >>> valenciaMin (creaGrafo [1..4] [(1,2),(1,3),(2,4)]) 
-- 1
valenciaMin :: Ord a => Grafo a -> Int
valenciaMin g = minimum [grado g v | v <- vertices g]
\end{code}

\begin{definicion}
  Dado un grafo $G = (V,A)$ llamamos \textbf{valencia máxima} o \textbf{grado
  máximo} de $G$ al valor $\delta(G) = \max \{grad(v) | v \in V\}$
\end{definicion}

La función \texttt{(valenciaMax g)} devuelve la valencia máxima del grafo
\texttt{g}.

\index{\texttt{valenciaMax}}
\begin{code}
-- | Ejemplo
-- >>> valenciaMax (creaGrafo [1..4] [(1,2),(1,3),(2,4)]) 
-- 2
valenciaMax :: Ord a => Grafo a -> Int
valenciaMax g = maximum [grado g v | v <- vertices g]
\end{code}

\begin{definicion}
  Se dice que un grafo es simple si no contiene lazos ni aristas repetidas.
\end{definicion}

La función \texttt{(esSimple g)} se verifica si \texttt{g} es un grafo simple.

\index{\texttt{esSimple}}
\begin{code}
-- | Ejemplos
-- >>> esSimple (creaGrafo [1..3] [(1,2),(1,3),(2,3)])
-- True
-- >>> esSimple (creaGrafo [1..3] [(1,1),(1,2),(2,3)])
-- False
esSimple :: Ord a => Grafo a -> Bool
esSimple g =
  and [not ((x,x) `aristaEn` g) | x <- vertices g]
\end{code}

\begin{definicion}
  Sea $G$ un grafo. Llamamos \textbf{secuencia de grados} de G a la lista de
  grados de sus vértices. La secuencia se suele presentar en orden decreciente:
  $d_1 \geq d_2 \geq \dots \geq d_n$.
\end{definicion}

La función \texttt{(secuenciaGrados g)} devuelve la secuencia de los grados 
del grafo \texttt{g} en orden decreciente.

\index{\texttt{secuenciaGrados}}
\begin{code}
-- | Ejemplo
-- >>> secuenciaGrados (creaGrafo [1..5] [(1,2),(1,3),(1,4),(2,4)]) 
-- [3,2,2,1,0]
secuenciaGrados :: Eq a => Grafo a -> [Int]
secuenciaGrados g = sortBy (flip compare) [grado g v | v <- vertices g]
\end{code}

\begin{nota}
  ¿Qué listas de $n$ números enteros son secuencias de grafos de $n$ vértices?
  \begin{itemize}
  \item Si $\sum_{i=1}^{n} d_i$ es impar, no hay ninguno. 
  \item Si $\sum_{i=1}^{n} d_i$ es par, entonces siempre hay un grafo con 
    esa secuencia de grados (aunque no necesariamente simple).
  \end{itemize}
\end{nota}

\begin{definicion}
  Una \textbf{secuencia gráfica} es una lista de número enteros no negativos 
  que es la secuencia de grados para algún grafo simple.
\end{definicion}

La función \texttt{(secuenciaGrafica ss)} se verifica si existe algún 
grafo con la secuencia de grados \texttt{ss}.

\index{\texttt{secuenciaGrafica}}
\begin{code}
-- | Ejemplos
-- >>> secuenciaGrafica [3,2,2,1,0]
-- True
-- >>> secuenciaGrafica [3,2,2,2]
-- False
secuenciaGrafica :: [Int] -> Bool
secuenciaGrafica ss = even (sum ss) && all p ss
  where p s = s >= 0 && s <= length ss
\end{code}

\begin{definicion}
  Dado un grafo $G = (V,A)$, diremos que $G' = (V',A')$ es un \textbf{subgrafo}
  de $G$ si $V' \subseteq V$ y $A' \subseteq A$.
\end{definicion}

La función \texttt{(esSubgrafo g' g)} se verifica si \texttt{g'} es un
subgrafo de \texttt{g}.

\index{\texttt{esSubgrafo}}
\begin{code}
-- |Ejemplos
-- >>> esSubgrafo (bipartitoCompleto 3 2) (bipartitoCompleto 3 3)
-- True
-- >>> esSubgrafo (grafoEstrella 4) (grafoEstrella 5)
-- True
-- >>> esSubgrafo (completo 5) (completo 4)
-- False
-- >>> esSubgrafo (completo 3) (completo 4)
-- True
esSubgrafo :: Ord a => Grafo a -> Grafo a -> Bool
esSubgrafo g' g = 
  vertices g' `esSubconjunto` vertices g &&
  aristas g'  `esSubconjunto` aristas g 
\end{code}

\begin{definicion}
  Si $G' = (V',A')$ es un subgrafo de $G = (V,A)$ tal que $V' = V$, diremos que
  $G'$ es un \textbf{subgrafo maximal}, \textbf{grafo recubridor} o
  \textbf{grafo de expansión} (en inglés, \emph{spanning grah}) de $G$.
\end{definicion}

La función \texttt{(esSubgrafoMax g' g)} se verifica si \texttt{g'} es un
subgrafo maximal de \texttt{g}.

\index{\texttt{esSubgrafoMax}}
\begin{code}
-- | Ejemplos
-- >>> esSubgrafoMax (grafoRueda 3) (grafoRueda 4)
-- False
-- >>> esSubgrafoMax (grafoCiclo 4) (grafoRueda 4)
-- True
-- >>> esSubgrafoMax (creaGrafo [1..3] [(1,2)]) (grafoCiclo 3)
-- True
-- >>> esSubgrafoMax (creaGrafo [1..2] [(1,2)]) (grafoCiclo 3)
-- False
esSubgrafoMax :: Ord a => Grafo a -> Grafo a -> Bool
esSubgrafoMax g' g = 
  g' `esSubgrafo` g && conjuntosIguales (vertices g') (vertices g)
\end{code}

\begin{definicion}
  Sean $G' = (V',A')$, $G = (V,A)$ dos grafos si $V' \subset V$, o
  $A' \subset A$, se dice que $G'$ es un \textbf{subgrafo propio} de $G$, y se
  denota por $G' \subset G$.
\end{definicion}

La función \texttt{(esSubgrafoPropio g' g)} se verifica si \texttt{g'} es un
subgrafo propio de \texttt{g}.

\index{\texttt{esSubgrafoPropio}}
\begin{code}
-- | Ejemplos
-- >>> esSubgrafoPropio (grafoRueda 3) (grafoRueda 4)
-- True
-- >>> esSubgrafoPropio (grafoRueda 4) (grafoCiclo 5)
-- False
-- >>> esSubgrafoPropio (creaGrafo [1..3] [(1,2)]) (grafoCiclo 3)
-- True
-- >>> esSubgrafoPropio (creaGrafo [1..2] [(1,2)]) (grafoCiclo 3)
-- True
esSubgrafoPropio :: Ord a => Grafo a -> Grafo a -> Bool
esSubgrafoPropio g' g = 
  esSubgrafo g' g &&
  (not (conjuntosIguales (vertices g) (vertices g')) ||
   not (conjuntosIguales (aristas g) (aristas g')))
\end{code}

\subsection{Propiedades de grafos}

\begin{teorema}[Lema del apretón de manos]
  En todo grafo simple el número de vértices de grado impar es par o cero.
\end{teorema}

Vamos a comprobar que se verifica el lema del apretón de manos utilizando la
función \texttt{prop\_LemaApretonDeManos}.

\begin{sesion}
ghci> quickCheck prop_LemaApretonDeManos
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop$\_$LemaApretonDeManos}}
\begin{code}
prop_LemaApretonDeManos :: Grafo Int -> Bool
prop_LemaApretonDeManos g =
  even (length (filter odd [grado g v | v <- vertices g]))
  where g' = creaGrafo (vertices g)
                       [(x,y) | (x,y) <- aristas g, x /= y]

\end{code}

\begin{teorema}[Havel--Hakimi]
  Si $n > 1$ y $D = [d_1, \dots, d_n]$ es una lista de enteros, entonces $D$ es 
  secuencia gráfica si y sólo si la secuencia $D'$ obtenida borrando el mayor 
  elemento $d_{max}$ y restando 1 a los siguientes $d_{max}$ elementos más
  grandes es gráfica.
\end{teorema}

Vamos a comprobar que se verifica el teorema de Havel--Hakimi utilizando la
función \texttt{prop\_HavelHakimi}.

\begin{sesion}
ghci> quickCheck prop_HavelHakimi
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop$\_$HavelHakimi}}
\begin{code}
prop_HavelHakimi :: [Int] -> Bool
prop_HavelHakimi [] = True
prop_HavelHakimi (s:ss) = 
  not (secuenciaGrafica (s:ss) && not (esVacio ss)) ||
  secuenciaGrafica (map (\x -> x-1) (take s ss) ++ drop s ss) 
\end{code}

\subsection{Operaciones y propiedades sobre grafos}

\subsubsection{Eliminación de una arista}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sea $(u,v) \in A$. Definimos el grafo 
  $G \setminus (u,v)$ como el subgrafo de $G$, $G' = (V',A')$, con $V' = V$ y
  $A' = A \setminus \{(u,v)\}$. Esta operación se denomina \textbf{eliminar una
  arista}. 
\end{definicion}

La función \texttt{(eliminaArista g a)} elimina la arista \texttt{a}
del grafo \texttt{g}.

\index{\texttt{eliminaArista}}
\begin{code}
-- | Ejemplos
-- >>> eliminaArista (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) (1,4)
-- G [1,2,3,4] [(1,2),(2,4)]
-- >>> eliminaArista (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) (4,1)
-- G [1,2,3,4] [(1,2),(2,4)]
-- >>> eliminaArista (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) (4,3)
-- G [1,2,3,4] [(1,2),(1,4),(2,4)]
eliminaArista :: Ord a => Grafo a -> (a,a) -> Grafo a
eliminaArista g (a,b) =
  creaGrafo (vertices g) 
            (aristas g \\ [(a,b),(b,a)])
\end{code}

\subsubsection{Eliminación un vértice}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sea $v \in V$. Definimos el grafo $G \setminus v$
  como el subgrafo de $G$, $G' = (V',A')$, con $V' = V \setminus \{v\}$ y
  $A' = A \setminus \{a \in A | v \text{ es un extremo de }a\}$.  Esta
  operación se denomina \textbf{eliminar un vértice}.
\end{definicion}

La función \texttt{(eliminaVertice g v)} elimina el vértice \texttt{v}
del grafo \texttt{g}.

\index{\texttt{eliminaVertice}}
\begin{code}
-- | Ejemplos
-- >>> eliminaVertice (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) 1
-- G [2,3,4] [(2,4)]
-- >>> eliminaVertice (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) 4
-- G [1,2,3] [(1,2)]
-- >>> eliminaVertice (creaGrafo [1..4] [(1,2),(1,4),(2,4)]) 3
-- G [1,2,4] [(1,2),(1,4),(2,4)]
-- >>> eliminaVertice (creaGrafo [1..4] [(1,2),(1,4),(1,3)]) 1
-- G [2,3,4] []
eliminaVertice :: Ord a => Grafo a -> a -> Grafo a
eliminaVertice g v =
  creaGrafo (vertices g \\ [v]) 
            (as \\ [(a,b) | (a,b) <- as, a == v || b == v])
  where as = aristas g
\end{code}

\subsubsection{Suma de aristas}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sean $u,v \in V$ tales que
  $(u,v), (v,u) \not\in A$. Definimos el grafo $G + (u,v)$ como el grafo
  $G'=(V,A \cup \{(u,v)\})$. Esta operación se denomina \textbf{suma de una
  arista}.
\end{definicion}

La función \texttt{(sumaArista g a)} suma la arista \texttt{a} al grafo
\texttt{g}.

\index{\texttt{sumaArista}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 5
-- G [1,2,3,4,5] [(1,2),(1,5),(2,3),(3,4),(4,5)]
-- >>> sumaArista (grafoCiclo 5) (1,3)
-- G [1,2,3,4,5] [(1,2),(1,3),(1,5),(2,3),(3,4),(4,5)]
sumaArista :: Ord a => Grafo a -> (a,a) -> Grafo a
sumaArista g a = 
  creaGrafo (vertices g) (a : aristas g)
\end{code}

\subsubsection{Suma de vértices}

\begin{definicion}
  Sea $G = (V,A)$ un grafo y sea $v \not\in V$. Definimos el grafo $G + v$ como
  el grafo $G' = (V',A')$, donde $V'= V \cup \{v\}$,
  $A'=A \cup \{(u,v) | u \in V\}$.  Esta operación se denomina \textbf{suma de
    un vértice}.
\end{definicion}

La función \texttt{(sumaVertice g a)} suma el vértice \texttt{a} al grafo
\texttt{g}.

\index{\texttt{sumaVertice}}
\begin{code}
-- | Ejemplo
-- >>> grafoCiclo 3
-- G [1,2,3] [(1,2),(1,3),(2,3)]
-- >>> sumaVertice (grafoCiclo 3) 4
-- G [1,2,3,4] [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
sumaVertice :: Ord a => Grafo a -> a -> Grafo a
sumaVertice g v =
  creaGrafo (v : vs) (aristas g ++ [(u,v) | u <- vs]) 
  where vs = vertices g
\end{code}

\subsubsection{Propiedad de los grafos completos}

\begin{proposicion}
  La familia de grafos completos $K_n$ verifica que $K_n = K_{n-1}+n$.
\end{proposicion}

Vamos a ver que se cumple la propiedad utilizando la función
\texttt{prop$\_$completos}.

\begin{sesion}
ghci> quickCheck prop_completos
+++ OK, passed 100 tests.
\end{sesion}

\index{\texttt{prop$\_$completos}}
\begin{code}
prop_completos :: Int -> Property
prop_completos n = n >= 2 ==>
   completo n == sumaVertice (completo (n-1)) n
\end{code}

\comentario{A partir de esta propiedad, se puede dar una definición alternativa
  de $K_n$ (completo2) y comprobar su equivalencia con la primera (completo).}

\subsubsection{Suma de grafos}

\begin{definicion}
  Sean $G = (V,A), G' = (V',A)$ dos grafos. Definimos el grafo suma de
  $G$ y $G'$ como el grafo $G + G' = (V \cup V',\allowbreak 
             A \cup A' \cup \{(u,v) | u \in V, v \in V'\})$.  
  Esta operación se denomina \textbf{suma de grafos}.
\end{definicion}

La función \texttt{(sumaGrafos g g')} suma los grafos \texttt{g} y   
\texttt{g'}. 

\index{\texttt{sumaGrafos}}
\begin{code}
-- | Ejemplo
-- >>> let g1 = creaGrafo [1..3] [(1,1),(1,3),(2,3)]
-- >>> let g2 = creaGrafo [4..6] [(4,6),(5,6)]
-- >>> pp $ sumaGrafos g1 g2
-- G [1,2,3,4,5,6]
--   [(1, 1),(1, 3),(1, 4),(1, 5),(1, 6),(2, 3),(2, 4),
--    (2, 5),(2, 6),(3, 4),(3, 5),(3, 6),(4, 6),(5, 6)]
sumaGrafos :: Ord a => Grafo a -> Grafo a -> Grafo a
sumaGrafos g1 g2 =
  creaGrafo (vs1 `union` vs2)
            (aristas g1 `union` 
             aristas g2 `union` 
             [(u,v) | u <- vs1, v <- vs2])
  where vs1 = vertices g1
        vs2 = vertices g2
\end{code}

\subsubsection{Unión de grafos}

\begin{definicion}
  Sean $G = (V,A), G' = (V',A)$ dos grafos. Definimos el grafo unión de 
  $G$ y $G'$ como el grafo $G \cup H =(V \cup V',A \cup A')$.  
  Esta operación se denomina \textbf{unión de grafos}.
\end{definicion}

La función \texttt{(unionGrafos g g')} une los grafos \texttt{g} y   
\texttt{g'}. Por ejemplo,


\index{\texttt{sumaGrafos}}
\begin{code}
-- | Ejemplo
-- >>> let g1 = creaGrafo [1..3] [(1,1),(1,3),(2,3)]
-- >>> let g2 = creaGrafo [4..6] [(4,6),(5,6)]
-- >>> unionGrafos g1 g2
-- G [1,2,3,4,5,6] [(1,1),(1,3),(2,3),(4,6),(5,6)]
unionGrafos :: Ord a => Grafo a -> Grafo a -> Grafo a
unionGrafos g1 g2 =
  creaGrafo (vertices g1 `union` vertices g2)
            (aristas  g1 `union` aristas  g2)   
\end{code}

\subsubsection{Grafo complementario}

\begin{definicion}
  Dado un grafo $G = (V,A)$ se define el \textbf{grafo complementario} de $G$
  como $\overline{G} = (V,\overline{A})$, donde
  $\overline{A} = \{(u,v)| u,v \in V,(u,v) \not\in A\}$.
\end{definicion}

La función \texttt{(grafoComplementario g)} devuelve el grafo complementario de
\texttt{g}.

\index{\texttt{grafoComplementario}}
\begin{code}
-- | Ejemplo
-- >>> grafoComplementario (creaGrafo [1..3] [(1,1),(1,3),(2,3)])
-- G [1,2,3] [(1,2),(2,2),(3,3)]
grafoComplementario :: Ord a => Grafo a -> Grafo a 
grafoComplementario  g =
  creaGrafo vs
            [(u,v)| u <- vs, v <- vs, u <= v, not ((u,v) `aristaEn` g)]
  where vs = vertices g
\end{code}

\ignora{
  La validación es

  > doctest DefinicionesYPropiedades.lhs 
  Examples: 103  Tried: 103  Errors: 0  Failures: 0
}
