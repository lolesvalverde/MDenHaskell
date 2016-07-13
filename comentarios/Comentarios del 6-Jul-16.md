Buenos días Loles

Para que no aparezcan los ficheros `*.*~` escribe en el fichero de
configuración de emacs (`~/.emacs`) la expresión `(setq backup-inhibited t)`

En el módulo `DefinicionesYPropiedades.lhs`

+ No se necesita importar Data.Array

+ Hay que dejar espacios alrededor de los operadores. Por ejemplo, en lugar de
  `[u1==v1,u1==v2,u2==v1,u2==v2]` se escribe
  `[u1 == v1, u1 == v2, u2 == v1, u2 == v2]`

+ Como te comenté el día 3, al elegir el orden de los argumentos de las
  funciones se ponen de los que menos suelen variar a los que más. La razón es
  facilitar la parcialización. Además, ese orden suele corresponder al orden de
  fla definición matemática ("Sea G un rafo. Se dice que, ..."). Los tipos debe de ser
  
~~~  
  entorno   :: Grafo Int Int -> Int -> [Int]
  grado     :: Grafo Int Int -> Int -> Int
  esAislado :: Grafo Int Int -> Int -> Bool
~~~

+ La ecuación `entorno g v = adyacentes g v`se puede simplificar a `entorno = adyacentes` 

En `EjemplosGrafos.lhs` 

+ He cambiado los que quedaban de 0 a n-1 por de 1 a n.

+ Al empezar en 0, la definición de `grafoCirculante` se aleja de su especificación matemática. 

Saludos, José A.
