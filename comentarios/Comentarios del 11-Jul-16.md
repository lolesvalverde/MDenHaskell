Buenos días Loles

En la exportación del fichero `GrafoConListaDeAristas` no se debe de poner el
constructor porque impide el uso de tipos de datos. En la representación
actual, con `(G xs as)` `xs` es la lista de vértices, pero en otra puede ser un
conjunto. 

Cuando se hace abstracción es para basarse sólo en las funciones básicas y
poder cambiar sus definiciones sin que afecte su uso.

En el mismo fichero, tampoco se debe exportar `ejGrafo` que es una función
auxiliar que se usa en el módulo para construir los ejemplos.

En `Apendices.text` indicar la versión de la plataforma Haskell y de GHC que
estás utilizando. 

He hecho muchas modificaciones para trabajar sólo con la representación de
grafos como de listas de aristas.
 
Más adelante veremos si se puede usar una representación compatible con el
TAD pero que internamente trabaje con vectores o matrices de forma que lo único
que haya que cambiar sea la representación que se importa. Y otras
representaciones usando Data.Set y Data.Map.

Los códigos de la versión anterior está en
`codigo/Almacen/Version_con_vectores_y_matrices.zip`. 

Mira todo el texto revisado y continúa con los morfismos.

Saludos, José A.


