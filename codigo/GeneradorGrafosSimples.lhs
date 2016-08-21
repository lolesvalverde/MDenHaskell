En esta sección, presentaremos el generador de grafos que nos permitirá 
generar grafos simples como listas de aristas arbitrariamente y usarlos 
como ejemplos o para comprobar propiedades en la sección de matrices     
asociadas a grafos.

\ignora{
\begin{code}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module GeneradorGrafosSimples (grafoSimple) where

import Test.QuickCheck
import GrafoConListaDeAristas
\end{code}
}

\texttt{(grafoSimple)} es un generador de grafos simples de hasta 10
vértices. Por ejemplo,

\begin{sesion}
ghci> sample grafoSimple
G [1,2,3,4,5,6,7,8] [(1,4),(1,5),(1,6),(1,7),(1,8),(2,3),
   (2,4),(2,5),(2,7),(3,5),(3,7),(4,5),(4,6),(5,6),(6,7)]
G [1,2,3,4,5,6,7,8,9] [(1,4),(1,8),(2,3),(2,5),(2,9),
   (3,4),(3,5),(3,6),(3,8),(3,9),(4,6),(4,8),(4,9),
   (5,8),(5,9),(6,9),(7,8)]
G [1,2,3,4,5,6] [(1,2),(1,3),(2,4),(2,5),(2,6),(3,5),(5,6)]
G [1,2,3,4,5,6,7] [(1,2),(1,3),(1,4),(2,4),(2,5),(2,6),
   (3,6),(4,5),(4,6),(6,7)]
G [1,2,3,4] [(1,3),(1,4),(2,3),(3,4)]
G [1,2,3] [(2,3)]
G [1,2] [(1,2)]
G [1,2,3,4,5,6] [(1,6),(2,4),(2,6),(3,5),(3,6),(5,6)]
G [1,2,3,4,5,6,7,8,9] [(1,2),(1,4),(1,5),(1,6),(1,7),(2,4),
   (3,4),(3,7),(3,8),(3,9),(4,5),(4,7),(4,9),(5,6),(5,7),
   (5,8),(5,9),(6,7),(6,9),(7,8),(7,9)]
G [1,2,3,4,5,6,7,8,9] [(1,3),(1,4),(1,5),(1,6),(2,3),(2,4),
   (2,9),(3,5),(3,9),(4,5),(4,7),(4,8),(4,9),(5,7),(5,8),
   (5,9),(6,7),(6,8),(6,9),(7,9)]
G [1] []
\end{sesion}

\index{\texttt{grafoSimple}}
\begin{code}
grafoSimple :: Gen (Grafo Int)
grafoSimple = do
  n  <- choose (0,10)
  as <- sublistOf [(x,y) | x <- [1..n], y <- [x+1..n]] 
  return (creaGrafo [1..n] as)
\end{code}

\end{nota}
