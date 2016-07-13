\subsection{Morfismos de grafos y su implementación en Haskell} 

\begin{code}
module PreMorfismos where
  
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import Test.QuickCheck
import Data.List
import DefinicionesYPropiedades 
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORFISMOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un \textbf{morfismo}
  entre $G$ y $G'$ es una aplicación $\phi: V \to V'$ que conserva las
  adyacencias; es decir, $\forall u,v \in V$ tales que $(u,v)\in A$, entonces,
  ha de verificarse que $(\phi(u),\phi(v))\in A'$.
\end{definicion}

La función \texttt{(esMorfismo vvs g h)} se verifica si la
aplicación cuyo grafo es \texttt{vvs} es un morfismo entre los 
grafos \texttt{g} y \texttt{h}. Para definirla, hemos utilizado otras
dos funciones:
\begin{itemize}
  \item\texttt{(esFuncion vvs g h)} se verifica si la aplicación cuyo grafo 
       es \texttt{vvs} es una función entre los grafos \texttt{g} y \texttt{h}.

\index{\texttt{esFuncion}}
\begin{code}
esFuncion :: (Eq a,Eq b) => [(a,b)] -> Grafo a -> Grafo b -> Bool
esFuncion vvs (G vs1 as1) (G vs2 as2) =
   esBinaria && esFuncional
    where esBinaria = all (`elem` vs1) vv1 && all (`elem` vs2) vv2
          esFuncional = todosTienenImagen && laImagenEsUnica
          todosTienenImagen = all (`elem` vv1) vs1
          laImagenEsUnica = all p (map aux vv1)
          p (x:xs) = all (==x) xs
          aux x = [b | (a,b) <- vvs, a==x] 
          vv1 = map fst vvs
          vv2 = map snd vvs
\end{code}

\item\texttt{(conservaAdyacencia vvs g h)} se verifica si la aplicación cuyo
  grafo es \texttt{vvs} conserva la adyacencia.
  
\index{\texttt{conservaAdyacencia}}
\begin{code}
conservaAdyacencia :: (Eq a,Eq b) => 
                      [(a,b)] -> Grafo a -> Grafo b -> Bool
conservaAdyacencia vvs (G vs1 as1) (G vs2 as2) =
   all p (aplicaFuncion vvs as1) 
    where p (u,v) = elem (u,v) as2 || elem (v,u) as2
          aplicaFuncion vvs as = zip (aux1 (map fst as)) 
                                     (aux1 (map snd as))
          aux1 xs = map aux2 xs
          aux2 x = head [b | (a,b) <- vvs, a==x]  
\end{code}
\end{itemize}

\begin{sesion}
ghci> esMorfismo [(1,'a'),(2,'b'),(3,'c')] (grafoCiclo 3)
                 (creaGrafo ['a'..'c'] [('a','b'),('a','c'),('b','c')])
True
ghci> esMorfismo [(1,1),(2,3),(3,2)] (bipartitoCompleto 1 2)
                 (grafoCiclo 3)
True
ghci> esMorfismo [(1,3),(2,2),(2,2)] (bipartitoCompleto 1 2)
                 (grafoCiclo 3)
False
\end{sesion}

\index{\texttt{esMorfismo}}
\begin{code}
esMorfismo :: (Eq a,Eq b) => [(a,b)] -> Grafo a -> Grafo b -> Bool
esMorfismo vvs g@(G vs1 as1) h@(G vs2 as2) = 
   esFuncion vvs g h && conservaAdyacencia vvs g h
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
morfismos :: (Eq a, Eq b) => Grafo a -> Grafo b -> [[(a,b)]]
morfismos g@(G vs1 _) h@(G vs2 _) =
   [vss | vss <- todas, esMorfismo vss g h]
    where todas = [zip vs1 xs | xs <- variacionesR l vs2]
          l = genericLength vs1
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%% ISOMORFISMOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{definicion}
  Dados dos grafos simples $G = (V,A)$ y $G' = (V',A')$, un
  \textbf{isomorfismo} entre $G$ y $G'$ es un morfismo biyectivo
  cuyo inverso es morfismo entre $G'$ y $G$.
\end{definicion}

La función \texttt{(esIsomorfismo vvs g h)} se verifica si la
aplicación cuyo grafo es \texttt{vvs} es un isomorfismo entre los 
grafos \texttt{g} y \texttt{h}.

\begin{sesion}
ghci> esIsomorfismo [(1,'a'),(2,'b'),(3,'c')] (grafoCiclo 3)
                    (creaGrafo ['a'..'c']
                               [('a','b'),('a','c'),('b','c')])
True
ghci> esIsomorfismo [(1,1),(2,3),(3,2)] (bipartitoCompleto 1 2)
                    (grafoCiclo 3)
False
ghci> esIsomorfismo [(1,3),(2,2),(2,2)] (bipartitoCompleto 1 2)
                    (grafoCiclo 3)
False
\end{sesion}

\index{\texttt{esIsomorfismo}}
\begin{code}
esIsomorfismo :: (Eq a,Eq b) => [(a,b)] -> Grafo a -> Grafo b -> Bool
esIsomorfismo vvs g h@(G vs2 _) = 
   esMorfismo vvs g h && esBiyectivo && esMorfismo (inv vvs) h g
    where esBiyectivo = esInyectivo && esSobreyectivo
          esInyectivo = nub vv2 == vv2
          esSobreyectivo = all (`elem` vv2) vs2
          vv2 = map snd vvs
          inv [] = []
          inv ((u,v):vs) = (v,u):inv vs 
\end{code}
