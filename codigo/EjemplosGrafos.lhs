El objetivo de esta sección es reunir una colección de grafos lo
suficientemente extensa y variada como para poder utilizarla como recurso a la
hora de comprobar las propiedades y definiciones de funciones que
implementaremos más adelante.

En el proceso de recopilación de ejemplos, se ha trabajado con diversas fuentes:
\begin{itemize}
  \item los apuntes de la asignatura ``Matemática discreta'' (\cite{Cardenas-15a}),
  \item los temas de la asignatura ``Informática'' (\cite{Alonso-15a}) y
  \item el artículo ``Graph theory'' (\cite{Wikipedia-grafos}) de la Wikipedia.
\end{itemize}

\begin{nota}
Se utilizará la representación de los grafos como listas de aristas.
\end{nota}

\ignora{
\begin{code}
module EjemplosGrafos ( grafoNulo
                      , esGrafoNulo 
                      , grafoCiclo
                      , grafoAmistad
                      , completo
                      , bipartitoCompleto
                      , grafoEstrella
                      , grafoRueda
                      , grafoCirculante
                      , grafoPetersenGen
                      , grafoThomson
                      , grafoPetersen
                      , grafoMoebiusCantor
                      ) where

import Data.List ( sort )
import Text.PrettyPrint.GenericPretty (pp)
import GrafoConListaDeAristas ( Grafo
                              , aristas
                              , creaGrafo
                              , vertices
                              )
\end{code}
}

\subsection{Grafo nulo}

\begin{definicion}
  Un \textbf{grafo nulo} es un grafo que no tiene ni vértices ni aristas.
\end{definicion}

La función \texttt{(grafoNulo)} devuelve un grafo nulo.

\index{\texttt{grafoNulo}}
\begin{code}
grafoNulo :: Ord a => Grafo a
grafoNulo = creaGrafo [] []
\end{code}

La función \texttt{(esGrafoNulo g)} se verifica si \texttt{g} es un grafo nulo.

\index{\texttt{grafoNulo}}
\begin{code}
-- | Ejemplos
-- >>> esGrafoNulo grafoNulo
-- True
-- >>> esGrafoNulo (creaGrafo [] [(1,2)])
-- False
-- >>> esGrafoNulo (creaGrafo [1,2] [(1,2)])
-- False
esGrafoNulo :: Grafo a -> Bool
esGrafoNulo g =
  null (vertices g) && null (aristas g)
\end{code}

\subsection{Grafo ciclo}

\begin{definicion}
  Un \href{https://es.wikipedia.org/wiki/Grafo_ciclo}
          {\textbf{ciclo},}\
     \footnote{\url{https://es.wikipedia.org/wiki/Grafo_completo}}
  de orden n, $C(n)$, es un grafo no dirigido y no ponderado cuyo conjunto 
  de vértices viene dado por $V = \{1, \dots, n\}$ y el de las aristas por
  $A = \{(0,1), \allowbreak(1,2), \dots, \allowbreak(n-2,n-1),
  \allowbreak(n-1,0)\}$
\end{definicion}

La función \texttt{(grafoCiclo n)} nos genera el ciclo de orden \texttt{n}. 

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \GraphInit[vstyle=Shade]
  \grCycle[RA=1.5]{5}
  \AssignVertexLabel{a}{1,2,3,4,5}
\end{tikzpicture}
\end{center}

\index{\texttt{grafoCiclo}}
\begin{code}
-- | Ejemplos
-- >>> grafoCiclo 5
-- G [1,2,3,4,5] [(1,2),(1,5),(2,3),(3,4),(4,5)]
grafoCiclo :: Int -> Grafo Int
grafoCiclo 0 = grafoNulo
grafoCiclo 1 = creaGrafo [1] []              
grafoCiclo n = creaGrafo [1..n]
                         ([(u,u+1) | u <- [1..n-1]] ++ [(n,1)])
\end{code}

\subsection{Grafo de la amistad}

\begin{definicion}
  Un \href{https://es.wikipedia.org/wiki/Grafo_de_la_amistad}
          {\textbf{grafo de la amistad}}\
     \footnote{\url{https://es.wikipedia.org/wiki/Grafo_de_la_amistad}}
  de orden $n$ es un grafo con $2n+1$ vértices y $3n$ aristas formado
  uniendo $n$ copias del ciclo $C_3$
  por un vértice común. Lo denotamos por $F_n$.
\end{definicion}

La función \texttt{(grafoAmistad n)} genera el grafo de la amistad de orden
$n$.  Por ejemplo,

% EJEMPLO DE CÓMO DIBUJAR UN GRAFO CUALQUIERA
% Si queremos hacer uno circular, es más fácil indicar las coordenadas como
% (a:b) donde b es el radio del grafo y a es su dirección en grados.

\begin{center}
\begin{tikzpicture} 
   \tikzstyle{every node} = [draw, 
                             shape = circle,
                             ball color=orange,
                             minimum size = 24pt]
   \GraphInit[vstyle=Shade]
   \Vertex[x=2,y=1]{1};
   \Vertex[x=4,y=0]{2};
   \Vertex[x=4,y=2]{3};
   \Vertex[x=0,y=2]{4};
   \Vertex[x=0,y=0]{5};
   \Edge(1)(2)
   \Edge(1)(3)
   \Edge(1)(4)
   \Edge(1)(5)
   \Edge(2)(3)
   \Edge(4)(5);
\end{tikzpicture}
\end{center}

\begin{center}
\begin{tikzpicture} 
   \tikzstyle{every node} = [draw, 
                             shape = circle,
                             ball color=orange,
                             minimum size = 24pt]
   \GraphInit[vstyle=Shade]
   \node (1) at (  0:0){1};
   \node (2) at (330:2){2};
   \node (3) at ( 30:2){3};
   \node (4) at ( 90:2){4};
   \node (5) at (150:2){5};
   \node (6) at (210:2){6};
   \node (7) at (270:2){7};
   \Edge(1)(2)
   \Edge(1)(3)
   \Edge(1)(4)
   \Edge(1)(5)
   \Edge(1)(6)
   \Edge(1)(7)
   \Edge(2)(3)
   \Edge(4)(5)
   \Edge(6)(7);
\end{tikzpicture}
\end{center}

\index{\texttt{grafoAmistad}}
\begin{code}
-- | Ejemplos
-- >>> pp $ grafoAmistad 2
-- G [1,2,3,4,5]
--   [(1, 2),(1, 3),(1, 4),(1, 5),(2, 3),(4, 5)]
-- >>> pp $ grafoAmistad 3
-- G [1,2,3,4,5,6,7]
--   [(1, 2),(1, 3),(1, 4),(1, 5),(1, 6),(1, 7),(2, 3),
--    (4, 5),(6, 7)]
grafoAmistad :: Int -> Grafo Int 
grafoAmistad n =
  creaGrafo [1..2*n+1]
            ([(1,a) | a <- [2..2*n+1]] ++
             [(a,b) | (a,b) <-zip [2,4..2*n] [3,5..2*n+1]])
\end{code}

\subsection{Grafo completo}

\begin{definicion}
  El \href{https://es.wikipedia.org/wiki/Grafo_completo}
          {\textbf{grafo completo},}\
     \footnote{\url{https://es.wikipedia.org/wiki/Grafo_completo}}
  de orden $n$, $K(n)$, es un grafo no dirigido cuyo conjunto de vértices 
  viene dado por $V = \{1, \dots, n\}$ y tiene una arista entre cada par 
  de vértices distintos.
\end{definicion}

La  función \texttt{(completo n)} nos genera el grafo completo de orden
$n$. Por ejemplo,

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \GraphInit[vstyle=Shade]
  \grComplete[RA=1.5]{4}
  \AssignVertexLabel{a}{1,2,3,4}
\end{tikzpicture}
\end{center}

\index{\texttt{completo}}
\begin{code}
-- | Ejemplo
-- >>> completo 4
-- G [1,2,3,4] [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
completo :: Int -> Grafo Int
completo n =
  creaGrafo [1..n]
            [(a,b) | a <- [1..n], b <- [1..a-1]]
\end{code}

\subsection{Grafo bipartito}

\begin{definicion}
  Un \href{https://es.wikipedia.org/wiki/Grafo_bipartito}
          {\textbf{grafo bipartito}}\
     \footnote{\url{https://es.wikipedia.org/wiki/Grafo_bipartito}}
  es un grafo $G=(V,A)$ verificando que el conjunto de sus vértices se puede
  dividir en dos subconjuntos disjuntos $V_1,V_2$ tales que
  $V_1\cup V_2 = V$ de manera que
  $\forall u_1,u_2 \in V_1[(u_1,u_2) \not\in A]$ y
  $\forall v_1,v_2 \in V_2[(v_1,v_2) \not\in A]$.

  Un \href{https://es.wikipedia.org/wiki/Grafo_bipartito_completo}
                {\textbf{grafo bipartito completo}}\
       \footnote{\url{https://es.wikipedia.org/wiki/Grafo_bipartito_completo}}
  será entonces un grafo bipartito $G=(V_1\cup V_2,A)$ en el que todos los
  vértices de una partición están conectados a los de la otra. Si $n=|V_1|, 
  m=|V_2|$ denotamos al grafo bipartito $G=(V_1\cup V_2,A)$ por
  \textbf{$K_{n,m}$}. 
\end{definicion}

La función \texttt{(bipartitoCompleto n m)} nos genera el grafo bipartito
$K_{n,m}$. Por ejemplo,

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \GraphInit[vstyle=Shade]
  \grCompleteBipartite[RA=1.5,RB=1.5,RS=2]{2}{3}
  \AssignVertexLabel{a}{1,2}
  \AssignVertexLabel{b}{3,4,5}
\end{tikzpicture}
\end{center}

\index{\texttt{bipartitoCompleto}}
\begin{code}
-- | Ejemplo
-- >>> bipartitoCompleto 2 3
-- G [1,2,3,4,5] [(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
bipartitoCompleto :: Int -> Int -> Grafo Int 
bipartitoCompleto n m =
  creaGrafo [1..n+m]
            [(a,b) | a <- [1..n], b <- [n+1..n+m]]
\end{code}

\subsection{Grafo estrella}

\begin{definicion}
  Una \href{https://en.wikipedia.org/wiki/Star_(graph_theory)}
           {\textbf{estrella}}\
      \footnote{\url{https://en.wikipedia.org/wiki/Star_(graph_theory))}}
  de orden $n$ es el grafo bipartito completo $K_{1,n}$. Denotaremos a una
  estrella de orden $n$ por $S_n$. Una estrella con 3 aristas se conoce en
  inglés como \textbf{claw} (garra o garfio).
\end{definicion}

La función \texttt{(grafoEstrella n)} crea un grafo circulante a partir de su
orden $n$. 

\begin{center}
\begin{tikzpicture}
  \GraphInit[vstyle=Shade]
  \SetVertexNoLabel
  \grStar[RA=1.5]{6}
  \AssignVertexLabel{a}{2,3,4,5,6,1}
\end{tikzpicture}
\end{center}

\index{\texttt{grafoEstrella}}
\begin{code}
-- | Ejemplo
-- >>> grafoEstrella 5
-- G [1,2,3,4,5,6] [(1,2),(1,3),(1,4),(1,5),(1,6)]
grafoEstrella :: Int -> Grafo Int
grafoEstrella = bipartitoCompleto 1 
\end{code}

\subsection{Grafo rueda}

\begin{definicion}
  Un \href{https://es.wikipedia.org/wiki/Grafo_rueda}
          {\textbf{grafo rueda}}\
     \footnote{\url{https://es.wikipedia.org/wiki/Grafo_rueda}}
  de orden $n$ es un grafo no dirigido y no ponderado con $n$ vértices que se
  forma conectando un único vértice a todos los vértices de un ciclo
  $C_{n-1}$. Lo denotaremos por $W_{n}$.
\end{definicion}

La función \texttt{(grafoRueda n)} crea un grafo rueda a partir de su
orden \texttt{n}. 

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \GraphInit[vstyle=Shade]
  \grWheel[prefix= ,RA=1.5]{6}
  \AssignVertexLabel{a}{2,3,4,5,6,1}
\end{tikzpicture}
\end{center}

\index{\texttt{grafoRueda}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoRueda 6
-- G [1,2,3,4,5,6]
--   [(1, 2),(1, 3),(1, 4),(1, 5),(1, 6),(2, 3),(2, 6),
--    (3, 4),(4, 5),(5, 6)]
grafoRueda :: Int -> Grafo Int 
grafoRueda n =
  creaGrafo [1..n]
            ([(1,a) | a <- [2..n]] ++
             [(a,b) | (a,b) <- zip (2:[2..n-1]) (3:n:[4..n])])
\end{code}

\subsection{Grafo circulante}

\begin{definicion}
  Un \href{https://en.wikipedia.org/wiki/Circulant_graph}
          {\textbf{grafo circulante}}\
          \footnote{\url{https://en.wikipedia.org/wiki/Circulant_graph}}
  de orden $n\geq3$ y saltos $\{s_1,\dots,s_k\}$ es un grafo no dirigido y no
  ponderado $G = (\{1, \dots, n\},A)$ en el que cada nodo $\forall i \in V$ es
  adyacente a los $2k$ nodos $i \pm s_1, \dots, i \pm s_k \mod n$. Lo
  denotaremos por $Cir_{n}^{s_1,\dots,s_k}$.
\end{definicion}

La función \texttt{(grafoCirculante n ss)} crea un grafo circulante a partir de
su orden \texttt{n} y de la lista de sus saltos \texttt{ss}. Por ejemplo,

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \GraphInit[vstyle=Shade]
  \grCirculant[RA=1.5]{6}{1,2}
  \AssignVertexLabel{a}{2,3,4,5,6,1}
\end{tikzpicture}
\end{center}

\index{\texttt{grafoCirculante}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoCirculante 6 [1,2]
-- G [1,2,3,4,5,6]
--   [(1, 2),(1, 3),(1, 5),(1, 6),(2, 3),(2, 4),(2, 6),
--    (3, 4),(3, 5),(4, 5),(4, 6),(5, 6)]
grafoCirculante :: Int -> [Int] -> Grafo Int
grafoCirculante n ss =
  creaGrafo [1..n]
            [(a,b) | a <- [1..n]
                   , b <- sort (auxCir a ss n)
                   , a < b]
  where auxCir v ss1 k =
          concat [[fun (v+s) k, fun (v-s) k] | s <- ss1]
        fun a b = if mod a b == 0 then b else mod a b
\end{code}

\subsection{Grafo de Petersen generalizado}

El \href{https://en.wikipedia.org/wiki/Generalized_Petersen_graph}
   {\textbf{grafo de Petersen generalizado}}\
   \footnote{\url{https://en.wikipedia.org/wiki/Generalized_Petersen_graph}}
que denotaremos $GP_{n,k}$ (con $n \geq 3$ y $1 \leq k \leq (n-1)/2$) es un 
grafo formado por un grafo circulante $Cir^n_{\{ k \}}$ en el interior, 
rodeado por un ciclo $C_n$ al que está conectado por una arista saliendo 
de cada vértice, de forma que se creen $n$ polígonos regulares. El grafo
$GP_{n,k}$ tiene $2n$ vértices y $3n$ aristas.

La función \texttt{(grafoPetersenGen n k)} devuelve el grafo de Petersen
generalizado $GP_{n,k}$.

\begin{center}
\begin{tikzpicture}[rotate=90]
  \GraphInit[vstyle=Shade]
  \SetVertexNoLabel
  \grGeneralizedPetersen[Math,RA=2.5,RB=1]{4}{2}
  \AssignVertexLabel{b}{1,2,3,4}
  \AssignVertexLabel{a}{5,6,7,8}
\end{tikzpicture}
\end{center}

\index{\texttt{grafoPetersenGen}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoPetersenGen 4 2
-- G [1,2,3,4,5,6,7,8]
--   [(1, 3),(1, 5),(2, 4),(2, 6),(3, 7),(4, 8),(5, 6),
--    (5, 8),(6, 7),(7, 8)]
grafoPetersenGen :: Int -> Int -> Grafo Int
grafoPetersenGen n k =
  creaGrafo [1..2*n]
            (filter p (aristas (grafoCirculante n [k])) ++
             [(x,x+n) | x <- [1..n]] ++
             (n+1,n+2) : (n+1,2*n) : [(x,x+1) | x <- [n+2..2*n-1]])
  where p (a,b) = a < b
\end{code}

\subsection{Otros grafos importantes}

\subsubsection{Grafo de Thomson}

\begin{definicion}
  El grafo bipartito completo $K_{3,3}$ es conocido como el \textbf{grafo de
  Thomson} y, como veremos más adelante, será clave a la hora de analizar
  propiedades topológicas de los grafos.
\end{definicion}

\begin{center}
\begin{tikzpicture}
  \SetVertexNoLabel
  \tikzset{VertexStyle/.style = {shape           = circle,
                                 shading         = ball,
                                 ball color      = green!60!black,
                                 minimum size    = 24pt,
                                 draw}}
  \tikzset{EdgeStyle/.style = {thick,%
                               double           = orange,
                               double distance  = 1pt}} 
  \grCompleteBipartite[RA=1.5,RB=1.5,RS=2]{3}{3}
  \AssignVertexLabel{b}{1,2,3}
  \AssignVertexLabel{a}{4,5,6}
\end{tikzpicture}
\end{center}

La función \texttt{(grafoThomson)} genera el grafo de Thomson.

\index{\texttt{grafoThomson}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoThomson
-- G [1,2,3,4,5,6]
--   [(1, 4),(1, 5),(1, 6),(2, 4),(2, 5),(2, 6),(3, 4),
--    (3, 5),(3, 6)]
grafoThomson :: Grafo Int
grafoThomson = bipartitoCompleto 3 3
\end{code}

\subsubsection{Grafo de Petersen}

El \href{https://en.wikipedia.org/wiki/Petersen_graph}
          {\textbf{grafo de Petersen}}\
          \footnote{\url{https://en.wikipedia.org/wiki/Petersen_graph}}
es un grafo no dirigido con 10 vértices y 15 aristas que es usado como 
ejemplo y como contraejemplo en muchos problemas de la Teoría de grafos.

\begin{center}
\begin{tikzpicture}[rotate=90]
  \tikzset{VertexStyle/.style = {shape           = circle,
                                 shading         = ball,
                                 ball color      = green!60!black,
                                 minimum size    = 24pt,
                                 draw}}
  \tikzset{EdgeStyle/.style = {thick,%
                               double           = orange,
                               double distance  = 1pt}} 
  \SetVertexNoLabel
  \grGeneralizedPetersen[Math,RA=2.5,RB=1]{5}{2}
  \AssignVertexLabel{a}{6,7,8,9,10}
  \AssignVertexLabel{b}{1,2,3,4,5}
\end{tikzpicture}
\end{center}

La función \texttt{grafoPetersen} devuelve el grafo de Petersen.

\index{\texttt{grafoPetersen}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoPetersen
-- G [1,2,3,4,5,6,7,8,9,10]
--   [(1, 3),(1, 4),(1, 6),(2, 4),(2, 5),(2, 7),(3, 5),
--    (3, 8),(4, 9),(5, 10),(6, 7),(6, 10),(7, 8),(8, 9),
--    (9, 10)]
grafoPetersen :: Grafo Int
grafoPetersen = grafoPetersenGen 5 2
\end{code}

\subsubsection{Grafo de Moëbius--Cantor}

El \href{https://en.wikipedia.org/wiki/Moëbius-Kantor_graph}
        {\textbf{grafo de Moëbius--Cantor}}\
        \footnote{\url{https://en.wikipedia.org/wiki/Moëbius-Kantor_graph}} 
se define como el grafo de Petersen generalizado $GP_{8,3}$; es decir,
está formado por los vértices de un octógono, conectados a los vértices
de una estrella de ocho puntas en la que cada nodo es adyacente a los
nodos que están a un salto 3 de él.  Al igual que el grafo de Petersen,
tiene importantes propiedades que lo hacen ser ejemplo y contraejemplo
de muchos problemas de la Teoría de Grafos.

\begin{center}
\begin{tikzpicture}
   \tikzset{VertexStyle/.style=   { shape        = circle,
                                    shading      = ball,
                                    ball color   = green!60!black,
                                    minimum size = 24pt,
                                    draw}}
   \tikzset{EdgeStyle/.style=   {thick,
                                 double          = orange,
                                 double distance = 1pt}}
   \SetVertexNoLabel
   \grGeneralizedPetersen[Math,RA=3,RB=2]{8}{3}   
   \AssignVertexLabel{a}{9,10,11,12,13,14,15,16}
   \AssignVertexLabel{b}{1,2,3,4,5,6,7,8}
\end{tikzpicture}
\end{center}

La función \texttt{grafoMoebiusCantor} genera el grafo de Moëbius--Cantor

\index{\texttt{grafoMoebiusCantor}}
\begin{code}
-- | Ejemplo
-- >>> pp $ grafoMoebiusCantor
-- G [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
--   [(1, 4),(1, 6),(1, 9),(2, 5),(2, 7),(2, 10),(3, 6),
--    (3, 8),(3, 11),(4, 7),(4, 12),(5, 8),(5, 13),(6, 14),
--    (7, 15),(8, 16),(9, 10),(9, 16),(10, 11),(11, 12),
--    (12, 13),(13, 14),(14, 15),(15, 16)]
grafoMoebiusCantor :: Grafo Int
grafoMoebiusCantor = grafoPetersenGen 8 3
\end{code}

\ignora{
  La validación es

  > doctest EjemplosGrafos.lhs
  Examples: 22  Tried: 22  Errors: 0  Failures: 0
}
