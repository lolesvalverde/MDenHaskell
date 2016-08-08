En esta sección, presentaremos el generador de grafos que nos permitirá 
generar grafos como listas de aristas arbitrariamente y usarlos como 
ejemplos o para comprobar propiedades.

Para aprender a controlar el tamaño de los grafos generados, he     
consultado las siguientes fuentes:

\begin{itemize}
\item[*] \href{https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf}
  {\textbf{QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs}}\
  \footnote{\url{https://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf}}
(\cite{Quickcheck-testing1})

\item[*] \href{https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html}
  {\textbf{Property Testing using QuickCheck}}\
  \footnote{\url{https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html}}
(\cite{Quickcheck-testing2})
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

ghci> sample (generaGrafo 2)
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

\index{\texttt{generaGrafo}}
\begin{code}
generaGrafo :: Int -> Gen (Grafo Int)
generaGrafo s = do
  let m = s  `mod` 11
  n  <- choose (0,m)
  as <- sublistOf [(x,y) | x <- [1..n], y <- [x..n]] 
  return (creaGrafo [1..n] as)
\end{code}

\begin{nota}
  Los grafos están contenidos en la clase de los objetos generables
  aleatoriamente.

\begin{code}
instance Arbitrary (Grafo Int) where
  arbitrary = sized generaGrafo 
\end{code}
\end{nota}

En el siguiente ejemplo se pueden observar algunos grafos generados

\begin{sesion}
ghci> sample (arbitrary :: Gen (Grafo Int))
G [] []
G [1,2] [(1,1),(2,2)]
G [1] [(1,1)]
G [] []
G [] []
G [1,2,3] [(1,3),(3,3)]
G [1,2,3] [(1,1),(1,2),(1,3),(3,3)]
G [1,2] [(2,2)]
G [1] []
G [1,2] [(1,1),(1,2),(2,2)]
G [1] [(1,1)]
\end{sesion}
