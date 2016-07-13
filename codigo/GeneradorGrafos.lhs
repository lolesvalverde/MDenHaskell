En esta sección, presentaremos el generador de grafos que nos permitirá 
generar grafos como listas de aristas arbitrariamente y usarlos como 
ejemplos o para comprobar propiedades.

\begin{comentario}
Falta referencia al generador de grafos
\end{comentario}

\ignora{
\begin{code}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module GeneradorGrafos (generaGrafo) where

import Test.QuickCheck
import GrafoConListaDeAristas
\end{code}
}

\texttt{(generaGrafos)} es un generador de grafos de hasta 10 vértices. 
Por ejemplo,

\begin{sesion}
ghci> sample generaGrafo
G [1,2,3,4,5,6,7,8,9] [(1,1),(1,3),(1,5),(1,6),(1,7),(1,9),(2,6),(2,8),
                       (2,9),(3,4),(4,6),(4,7),(5,7),(5,9),(6,6),(6,7),
                       (7,7),(7,8),(7,9),(8,8),(8,9),(9,9)]
G [1,2] [(1,1),(2,2)]
G [1,2,3] [(1,3),(2,2)]
G [1,2,3,4,5,6,7,8] [(1,1),(1,5),(1,6),(1,7),(2,2),(2,5),(2,8),(3,3),
                     (3,5),(3,6),(3,8),(4,6),(5,6),(5,7),(5,8),(6,6),
                     (6,7)]
G [1,2,3,4,5,6,7] [(1,1),(1,3),(1,4),(1,7),(2,2),(2,5),(2,7),(3,4),
                   (3,5),(3,6),(4,4),(4,5),(4,6),(4,7),(5,6),(5,7),
                   (6,6),(6,7),(7,7)]
G [1,2] [(1,1),(1,2)]
G [1] []
G [1,2,3] [(1,1),(1,2),(2,3),(3,3)]
G [1,2] [(1,1),(2,2)]
G [1,2] []
G [1,2,3,4,5,6] [(1,3),(1,4),(1,6),(2,2),(2,4),(2,6),(3,4),
                 (3,5),(3,6),(4,4),(4,5),(5,5),(5,6)]
\end{sesion}

\index{\texttt{generaGrafo}}
\begin{code}
generaGrafo :: Gen (Grafo Int)
generaGrafo = do
  n <- choose (1,10)
  as <- sublistOf [(x,y) | x <- [1..n], y <- [x..n]]
  return (creaGrafo [1..n] as)
\end{code}

\begin{nota}
Los grafos están contenido en la clase de los objetos generables aleatoriamente. 

\begin{code}
instance Arbitrary (Grafo Int) where
    arbitrary = generaGrafo
\end{code}
\end{nota}
