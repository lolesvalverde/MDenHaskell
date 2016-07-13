Buenos días Loles

Los comentarios de la revisión de hoy

+ En las descripciones de las funciones, escribirlas entre paréntesis. Por
  ejemplo, en lugar de "La función \\texttt{bipartitoCompleto n m} nos genera el
  grafo" se escribe "La función \\texttt{(bipartitoCompleto n m)} nos genera el
  grafo".
  
+ La ecuación `grafoEstrella n = bipartitoCompleto 1 n` se puede simplificar
  a `grafoEstrella = bipartitoCompleto 1`
  
+ La ecuación `auxCir v [] n = []` se puede simplificar a `auxCir _ [] _ = []`

+ Puesto que `auxModula` y `auxCir`son funciones auxiliares de
  `grafoCirculante` sus tipos se pueden restringir a 
  
~~~
      auxModula :: Int -> Int -> Int
      auxCir :: Int -> [Int] -> Int -> [Int]
~~~

+ Se puede simplificar 

~~~
      auxModula :: Int -> Int -> Int
      auxModula m n | m >= 0 && m < n = m
                    | m >= 0          = auxModula (m-n) n
                    | otherwise       = auxModula (m+n) n
      
      auxCir :: Int -> [Int] -> Int -> [Int]
      auxCir _ []     _ = []
      auxCir v (s:ss) n = a : b : auxCir v ss n
            where a = auxModula (v-s) n
                  b = auxModula (v+s) n
~~~
  
por

~~~
      auxCir :: Int -> [Int] -> Int -> [Int]
      auxCir v ss n =
        concat [[(v+s) `mod` n, (v-s) `mod` n] | s <- ss]
~~~
  
Saludos, José A.

---

> Buenos días José Antonio,
> 
> mi primera idea fue usar directamente "mod" en vez de crear "auxModula", pero
> "mod" no admite números negativos. Si le cambio el tipo a "auxModula"
> entonces esta tampoco los admitirá. Puedo cambiar el tipo del segundo
> argumento que recibe, pero no del primero, porque existe la posibilidad de
> que este sea negativo. 
>
> No he terminado de entender el primer comentario. ¿Podría volver a comentar
> el problema con el formato, por favor? 
> 
> Muchas gracias,
> Loles 

---

Buenos días Loles

Hubo un error al procesar los comentarios. El primero era

+ En las descripciones de las funciones, escribirlas entre paréntesis. Por
  ejemplo, en lugar de "La función \\texttt{bipartitoCompleto n m} nos genera el
  grafo" se escribe "La función \\texttt{(bipartitoCompleto n m)} nos genera el
  grafo".
  
De todas formas, el archivo Markdown se encuentra en el Dropbox del TFG en
`comentarios/Comentarios del 2-Jul-16.md`.

Respecto del módulo, el orden del grafo es un entero positivo n. Por tanto,
(`mod` n) no dará problema.

Saludos, José A.

---

> Pero yo quiero tomar módulo para que a la hora de calcular los saltos, no me
> salgan "vértices negativos". Si el primer argumento que le doy a mod es
> negativo, no me devuelve ese mismo número módulo el segundo argumento. 

---

Buenos días Loles

Dime un ejemplo en el que no da lo que se desea.

Saludos, José A.

---




