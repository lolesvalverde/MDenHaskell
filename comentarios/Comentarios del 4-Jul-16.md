Buenos días Loles

Los comentarios de hoy son del módulo `DefinicionesYPropiedades.lhs`

+ La definición de valencia mínima hay que ajustarla para grafos dirigidos.

+ En los ejemplos, se suelen alinear los signos de igualdad.

+ En las siguientes expresiones se pueden eliminar los paréntesis:

~~~
      valenciaMin :: (Grafo Int Int) -> Int
      valenciaMin (grafoPetersen)   == 3
~~~

+ Hay que revisar el ejemplo `valenciaMin ejGrafoD == 0`. No hay ningún vértice
  aislado. 
  
+ La definición de valencia máxima hay que ajustarla para grafos dirigidos.  

+ Hay que revisar el ejemplo `valenciaMax ejGrafoD == 3`

Del módulo `GrafoConVectorDeAdyacencia.lhs`

+ En la signatura de los TAD se suelen dejar lo mínimo. Por ello, no es
  conveniente añadirle `ejGrafoD` y `ejGrafoND`. Si se desea usar, es mejor
  ponerlos en el fichero de ejemplos de grafos. Si lo que se quiere es tener
  ejemplos de grafos dirigidos, se puede añadir además el de los torneos.
  
  
