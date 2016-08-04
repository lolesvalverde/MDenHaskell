module MorfismosSpec where

import Conjuntos
import Relaciones
import Funciones
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import DefinicionesYPropiedades 
import Morfismos

import Test.Hspec
import Test.QuickCheck

ejemplosMorfismos :: Spec
ejemplosMorfismos =
  describe "Morfismos" $ do
  describe "conservaAdyacencia" $ do
    let g1 = creaGrafo [1..4] [(1,2),(2,3),(3,4)]
    let g2 = creaGrafo [1..4] [(1,2),(2,3),(2,4)]
    let g3 = creaGrafo [4,6..10] [(4,8),(6,8),(8,10)]
    it "e1" $ 
      conservaAdyacencia g1 g3 [(1,4),(2,6),(3,8),(4,10)] `shouldBe`
      False
    it "e2" $  
      conservaAdyacencia g2 g3 [(1,4),(2,8),(3,6),(4,10)] `shouldBe`
      True

  describe "esMorfismo" $ do
    let g1 = creaGrafo [1,2,3] [(1,2),(3,2)]
    let g2 = creaGrafo [4,5,6] [(4,6),(5,6)]
    it "e1" $
      esMorfismo g1 g2 [(1,4),(2,6),(3,5)] `shouldBe`
      True
    it "e2" $
      esMorfismo g1 g2 [(1,4),(2,5),(3,6)] `shouldBe`
      False
    it "e3" $
      esMorfismo g1 g2 [(1,4),(2,6),(3,5),(7,9)] `shouldBe`
      False

  describe "morfismos" $ do
    it "e1" $
      morfismos (grafoCiclo 3)
                (creaGrafo [4..6] [(4,5),(4,6),(5,6)]) `shouldBe`
      [[(1,4),(2,5),(3,6)],[(1,4),(2,6),(3,5)],[(1,5),(2,4),(3,6)],
       [(1,5),(2,6),(3,4)],[(1,6),(2,4),(3,5)],[(1,6),(2,5),(3,4)]]
    it "e2" $
      morfismos (bipartitoCompleto 1 2) (grafoCiclo 3) `shouldBe`
      [[(1,1),(2,2),(3,2)],[(1,1),(2,2),(3,3)],[(1,1),(2,3),(3,2)],
       [(1,1),(2,3),(3,3)],[(1,2),(2,1),(3,1)],[(1,2),(2,1),(3,3)],
       [(1,2),(2,3),(3,1)],[(1,2),(2,3),(3,3)],[(1,3),(2,1),(3,1)],
       [(1,3),(2,1),(3,2)],[(1,3),(2,2),(3,1)],[(1,3),(2,2),(3,2)]]
  
  describe "esIsomorfismo" $ do
    it "e1" $
      esIsomorfismo (grafoCiclo 3)
                    (creaGrafo ['a'..'c']
                               [('a','b'),('a','c'),('b','c')])
                    [(1,'a'),(2,'b'),(3,'c')] `shouldBe`
      True
    it "e2" $
      esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,1),(2,3),(3,2)] `shouldBe`
      False
    it "e3" $  
      esIsomorfismo (bipartitoCompleto 1 2) (grafoCiclo 3)
                    [(1,3),(2,2),(2,2)] `shouldBe`
      False

  describe "isomorfismos1" $ do
    it "e1" $
      isomorfismos1 (bipartitoCompleto 1 2) (grafoCiclo 3) `shouldBe`
      []
    it "e2" $
      isomorfismos1 (bipartitoCompleto 1 2) 
                    (creaGrafo "abc" [('a','b'),('b','c')]) `shouldBe`
      [[(1,'b'),(2,'a'),(3,'c')],[(1,'b'),(2,'c'),(3,'a')]]
    it "e3" $  
      isomorfismos1 (grafoCiclo 4) 
                    (creaGrafo [5..8] [(5,7),(5,8),(6,7),(6,8)]) `shouldBe`
      [[(1,6),(2,7),(3,5),(4,8)],[(1,5),(2,7),(3,6),(4,8)],
       [(1,7),(2,6),(3,8),(4,5)],[(1,8),(2,6),(3,7),(4,5)],
       [(1,5),(2,8),(3,6),(4,7)],[(1,6),(2,8),(3,5),(4,7)],
       [(1,8),(2,5),(3,7),(4,6)],[(1,7),(2,5),(3,8),(4,6)]]

  describe "isomorfos1" $ do
    it "e1" $
      isomorfos1 (grafoRueda 4) (completo 4) `shouldBe`
      True
    it "e2" $
      isomorfos1 (grafoRueda 5) (completo 5) `shouldBe`
      False
    it "e3" $
      isomorfos1 (grafoEstrella 2) (bipartitoCompleto 1 2) `shouldBe`
      True
    it "e4" $
      isomorfos1 (grafoCiclo 5) (bipartitoCompleto 2 3) `shouldBe`
      False

  describe "invariantrPosisomorfismos" $ do
    it "p1" $
      property $ (esInvariantePorIsomorfismos orden)
    it "p2" $
      property $ (esInvariantePorIsomorfismos tamaño)
    it "p3" $
      property $ (esInvariantePorIsomorfismos secuenciaGrados)

  describe "isomorfismos" $ do
    it "e1" $
      isomorfismos (bipartitoCompleto 1 2) (grafoCiclo 3) `shouldBe`
      []
    it "e2" $
      isomorfismos (bipartitoCompleto 1 2) 
                    (creaGrafo "abc" [('a','b'),('b','c')]) `shouldBe`
      [[(1,'b'),(2,'a'),(3,'c')],[(1,'b'),(2,'c'),(3,'a')]]
    it "e3" $  
      isomorfismos (grafoCiclo 4) 
                    (creaGrafo [5..8] [(5,7),(5,8),(6,7),(6,8)]) `shouldBe`
      [[(1,6),(2,7),(3,5),(4,8)],[(1,5),(2,7),(3,6),(4,8)],
       [(1,7),(2,6),(3,8),(4,5)],[(1,8),(2,6),(3,7),(4,5)],
       [(1,5),(2,8),(3,6),(4,7)],[(1,6),(2,8),(3,5),(4,7)],
       [(1,8),(2,5),(3,7),(4,6)],[(1,7),(2,5),(3,8),(4,6)]]

  describe "isomorfos" $ do
    it "e1" $
      isomorfos (grafoRueda 4) (completo 4) `shouldBe`
      True
    it "e2" $
      isomorfos (grafoRueda 5) (completo 5) `shouldBe`
      False
    it "e3" $
      isomorfos (grafoEstrella 2) (bipartitoCompleto 1 2) `shouldBe`
      True
    it "e4" $
      isomorfos (grafoCiclo 5) (bipartitoCompleto 2 3) `shouldBe`
      False

  describe "esAutomorfismo" $ do
    it "e1" $
      esAutomorfismo (bipartitoCompleto 1 2) [(1,2),(2,3),(3,1)] `shouldBe`
      False
    it "e2" $
      esAutomorfismo (bipartitoCompleto 1 2) [(1,1),(2,3),(3,2)] `shouldBe`
      True
    it "e3" $
      esAutomorfismo (grafoCiclo 4) [(1,2),(2,3),(3,4),(4,1)] `shouldBe`
      True

  describe "automorfismos" $ do
    it "e1" $
      automorfismos (grafoCiclo 4) `shouldBe`
      [[(1,1),(2,2),(3,3),(4,4)],[(1,3),(2,2),(3,1),(4,4)],
       [(1,4),(2,3),(3,2),(4,1)],[(1,2),(2,3),(3,4),(4,1)],
       [(1,4),(2,1),(3,2),(4,3)],[(1,2),(2,1),(3,4),(4,3)],
       [(1,1),(2,4),(3,3),(4,2)],[(1,3),(2,4),(3,1),(4,2)]]
    it "e2" $
      automorfismos (completo 4) `shouldBe`
      [[(1,1),(2,2),(3,3),(4,4)],[(1,2),(2,1),(3,3),(4,4)],
       [(1,3),(2,2),(3,1),(4,4)],[(1,2),(2,3),(3,1),(4,4)],
       [(1,3),(2,1),(3,2),(4,4)],[(1,1),(2,3),(3,2),(4,4)],
       [(1,4),(2,3),(3,2),(4,1)],[(1,3),(2,4),(3,2),(4,1)],
       [(1,3),(2,2),(3,4),(4,1)],[(1,4),(2,2),(3,3),(4,1)],
       [(1,2),(2,4),(3,3),(4,1)],[(1,2),(2,3),(3,4),(4,1)],
       [(1,4),(2,1),(3,2),(4,3)],[(1,1),(2,4),(3,2),(4,3)],
       [(1,1),(2,2),(3,4),(4,3)],[(1,4),(2,2),(3,1),(4,3)],
       [(1,2),(2,4),(3,1),(4,3)],[(1,2),(2,1),(3,4),(4,3)],
       [(1,4),(2,1),(3,3),(4,2)],[(1,1),(2,4),(3,3),(4,2)],
       [(1,1),(2,3),(3,4),(4,2)],[(1,4),(2,3),(3,1),(4,2)],
       [(1,3),(2,4),(3,1),(4,2)],[(1,3),(2,1),(3,4),(4,2)]]
    it "e3" $
      automorfismos (grafoRueda 5) `shouldBe`
      [[(1,1),(2,2),(3,3),(4,4),(5,5)],[(1,1),(2,4),(3,3),(4,2),(5,5)],
       [(1,1),(2,5),(3,2),(4,3),(5,4)],[(1,1),(2,3),(3,2),(4,5),(5,4)],
       [(1,1),(2,5),(3,4),(4,3),(5,2)],[(1,1),(2,3),(3,4),(4,5),(5,2)],
       [(1,1),(2,4),(3,5),(4,2),(5,3)],[(1,1),(2,2),(3,5),(4,4),(5,3)]]

verificaMorfismos :: IO ()
verificaMorfismos = 
  hspec ejemplosMorfismos

-- La validación es
--    ghci> verificaMorfismos
--    
--    Morfismos
--      conservaAdyacencia
--        e1
--        e2
--      esMorfismo
--        e1
--        e2
--        e3
--      morfismos
--        e1
--        e2
--      esIsomorfismo
--        e1
--        e2
--        e3
--      isomorfismos1
--        e1
--        e2
--        e3
--      isomorfos1
--        e1
--        e2
--        e3
--        e4
--      invariantrPosisomorfismos
--        p1
--        p2
--        p3
--      isomorfismos
--        e1
--        e2
--        e3
--      isomorfos
--        e1
--        e2
--        e3
--        e4
--      esAutomorfismo
--        e1
--        e2
--        e3
--      automorfismos
--        e1
--        e2
--        e3
--    
--    Finished in 0.0391 seconds
--    33 examples, 0 failures

