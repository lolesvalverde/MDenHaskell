Buenos días Loles

Los comentarios de la revisión de hoy son

+ En LaTeX. en lugar de `...` se escribe `\dots`

+ He añadido `\index{\texttt{completo}}` antes de la definición de
  `completo`. Lo mismo hay que hacer con todas las definiciones de funciones. 

+ En el módulo `EjemplosGrafos.lhs` 
    + De momento, no se necesita: 
         + `{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}`.
         + `import Data.Array`
         + `import Data.List (nub)`
         + `import Test.QuickCheck`
    + Los grafos `g1`, ..., `g12` no se usan.
    
+ En lugar de `(g ! (v, v')) /= Nothing`  se puede escribir `isJust (g ! (v, v'))`. 

+ La ayuda de las funciones de Haskell puedes verla con
  [Hoogle](https://www.haskell.org/hoogle). Por ejemplo, la de `isJust` se
  encuentra
  [aquí](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html#v:isJust)
  
Saludos, José A.

