Buenos días Loles

Los comentarios de la revisión de hoy son

En `DefinicionesYPropiedades.lhs`

+ La importación de `Data.List`, de momento, no es necesaria. La he borrado.

+ Poner el enlace al tema; es decir
  `https://dl.dropboxusercontent.com/u/15420416/tiddly/emptyMD1314.html#Chapter2`
  en lugar de
  `https://dl.dropboxusercontent.com/u/15420416/tiddly/emptyMD1314.html`
  
+ He puesto que se usa el TAD con referencia a la sección: *"Se utilizará el
  tipo abstracto de grafos presentados en la sección \\ref{sec:TAD_grafos}."*
  Para usar referencia, le he añadido la etiqueta `\\label{sec:TAD_grafos}` al
  comienzo de la sección "El TAD de los grafos".

+ Separar las definiciones matemáticas para que se correspondan con sus
  representaciones en Haskell.
  
+ En los ejemplos, debes de incluir también grafos no dirigidos. 

+ La definición `entornos` es incorrecta. Falla para los grafos dirigidos. Por
  ejemplo,

~~~  
      λ> let g3 = creaGrafo D (1,3) [(1,2,0),(2,2,0),(3,1,0),(3,2,0)]
      λ> entorno 2 g3
      [2]
~~~  

+ La definición correspondiente a `entorno` en la relación 20 de I1M es
  `incidentes`. 

+ Al elegir el orden de los argumentos de las funciones se ponen de los que
  menos suelen variar a los que más. La razón es facilitar la
  parcialización. Los tipos debe de ser
  
~~~  
  entorno   :: Grafo Int Int -> Int -> [Int]
  grado     :: Grafo Int Int -> Int -> Int
  esAislado :: Grafo Int Int -> Int -> Bool
~~~
 
+ Cuando los ejemplos caben en una línea, usar `==` 

En `EjemplosGrafos.lhs`

+ En la especificación de los grafos hay que incluir los argumentos (por
  ejemplo, `\\texttt{(grafoCirculante n ss)}` en lugar de
  `\\texttt{(grafoCirculante)}`) y usarlos en la descripción.
  
+ Dejar un espacio alrededor de los operadores. Por ejemplo, en lugar de `a<b`
  se escribe `a < b`.
  
En `GrafoConMatrizDeAdyacencia.lhs`

+ Simplificar los ejemplos, sin dividir los entornos y reduciéndolos a una
  línea.
  
Saludos, José A.



  
