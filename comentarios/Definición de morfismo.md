Buenas tardes Loles

Algunos comentarios sobre tu primera definición de morfismos de grafos:

~~~
      esMorfismo :: Eq a => [(a,a)] -> Grafo a -> Grafo a -> Bool
      esMorfismo vvs (G vs1 as1) (G vs2 as2) =
         all p (cambioTotal vvs as1) 
          where p (u,v) = elem (u,v) as2 || elem (v,u) as2
      
      cambioTotal :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
      cambioTotal [] as = as
      cambioTotal ((u,v):vs) as = cambioTotal vs (cambia (u,v) as)
      
      cambia :: Eq a => (a, a) -> [(a, a)] -> [(a, a)]
      cambia _ [] = []
      cambia (u,v) ((a,b):xs) | a == u    = cambia (u,v) ((v,b): xs)
                              | b == u    = cambia (u,v) ((a,v): xs)
                              | otherwise = (a,b):cambia (u,v) xs
~~~

+ El tipo debería de ser 

~~~
      esMorfismo :: (Ord a, Ord b) => Grafo a -> Grafo b -> [(a,b)] -> Bool
~~~

ya que el tipo de los vértices pueden ser distintos. Respecto del orden,
siempre es de menos a más cambiable (como en "Dados dos grafos simples $G =
(V,A)$ y $G' = (V',A')$, un morfismo entre $G$ y $G'$ ...).

Para la definición sería conveniente seguir la especificación matemática

~~~
      esMorfismo :: (Ord a, Ord b) => Grafo a -> Grafo b -> [(a,b)] -> Bool
      esMorfismo (G vs1 as1) (G vs2 as2) f =
        esFuncion vs1 vs2 && conservaAdyacencia f
        where conservaAdyacencia f = undefined
      
      esFuncion :: (Eq a, Eq b) :: [a] -> [b] -> [(a,b)] -> Bool
      esFuncion xs ys f = undefined
~~~

Saludos, José A.
