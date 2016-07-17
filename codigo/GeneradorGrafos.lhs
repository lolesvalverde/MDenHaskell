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
\end{sesion}

\index{\texttt{generaGrafo}}
\begin{code}
generaGrafo :: Int -> Gen (Grafo Int)
generaGrafo 0 = return (creaGrafo [] [])
generaGrafo s = do
  n <- choose (1,10)
  as <- sublistOf
        [(x,y) | x <- [1..(min s n)], y <- [x..(min s n)]] 
  return (creaGrafo [1..(min s n)] as)
\end{code}

\begin{nota}
Los grafos están contenido en la clase de los objetos 
generables aleatoriamente. 

\begin{code}
instance Arbitrary (Grafo Int) where
    arbitrary = sized generaGrafo 
\end{code}
\end{nota}
