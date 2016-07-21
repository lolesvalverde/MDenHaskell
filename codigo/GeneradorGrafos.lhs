En esta sección, presentaremos el generador de grafos que nos permitirá 
generar grafos como listas de aristas arbitrariamente y usarlos como 
ejemplos o para comprobar propiedades.

Para aprender a controlar el tamaño de los grafos generados, he     
consultado las siguientes fuentes:

\begin{itemize}
\item \href{https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf}
  {\textbf{QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs}}\
  \footnote{\url{https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf}}

\item \href{https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html}
  {\textbf{QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs}}\
  \footnote{\url{https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html}}
\end{itemize}

\ignora{
\begin{code}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module GeneradorGrafos (generaGrafo) where

import Test.QuickCheck
import GrafoConListaDeAristas
\end{code}
}

\texttt{(generaGrafos n)} es un generador de grafos de hasta \texttt{n}
vértices. Por ejemplo,

\begin{sesion}
ghci> sample (generaGrafo 5)
G [1,2] [(1,1),(1,2),(2,2)]
G [1,2,3,4] [(1,1),(1,3),(1,4),(2,3),(3,3)]
G [1,2,3,4] [(1,1),(1,2),(2,3),(2,4),(3,3),(3,4)]
G [1,2,3,4,5] 
  [(1,1),(1,2),(1,3),(1,4),(1,5),(2,3),(2,5),(3,5),(5,5)]
G [1,2,3,4,5] 
  [(1,1),(1,2),(1,3),(1,4),(1,5),(2,5),(3,3),(3,5),(4,4),(4,5)]
G [1,2] []
G [1,2,3] [(1,1),(2,3)]
G [1,2,3,4,5] 
  [(1,1),(1,2),(1,5),(2,2),(2,3),(2,4),(3,5),(5,5)]
G [1,2,3,4,5] [(1,3),(2,5),(3,3),(3,5),(5,5)]
G [1,2,3,4,5] [(1,1),(1,3),(1,5),(2,3),(2,5),(3,4)]
G [1,2,3,4,5] 
  [(1,1),(1,3),(1,4),(1,5),(2,5),(3,4),(3,5),(4,4),(4,5),(5,5)]

λ> sample (generaGrafo 2)
G [1] []
G [1] []
G [1] [(1,1)]
G [1] []
G [1] [(1,1)]
G [1,2] [(1,1),(2,2)]
G [1] [(1,1)]
G [1] []
G [1,2] [(1,1)]
G [1] []
G [] []
\end{sesion}

\comentario{Ver el 1 del 21-jul}

\index{\texttt{generaGrafo}}
\begin{code}
generaGrafo :: Int -> Gen (Grafo Int)
generaGrafo s = do
  n  <- choose (0,s)
  as <- sublistOf [(x,y) | x <- [1..n], y <- [x..n]] 
  return (creaGrafo [1..n] as)
\end{code}

\comentario{Ver 2 y 3 de 21-jul}

\begin{nota}
Los grafos están contenido en la clase de los objetos 
generables aleatoriamente. 

\begin{code}
instance Arbitrary (Grafo Int) where
    arbitrary = sized generaGrafo 
\end{code}
\end{nota}

En el siguiente ejemplo se pueden observar algunos grafos generados

\begin{sesion}
λ> sample (arbitrary :: Gen (Grafo Int))
G [] []
G [1,2] [(1,2)]
G [] []
G [1,2,3,4,5] [(1,3),(1,5),(2,3),(2,4),(3,4),(4,5),(5,5)]
G [1,2,3,4,5,6] [(1,2),(1,5),(3,3),(4,5),(4,6),(5,5),(5,6)]
G [1,2,3,4] [(1,1),(1,2),(1,3),(1,4),(2,3),(3,3)]
G [1,2,3,4,5,6,7,8,9,10,11,12,13,14] [(1,1),(1,3),(2,5),(3,4),(9,11)]
\end{sesion}
