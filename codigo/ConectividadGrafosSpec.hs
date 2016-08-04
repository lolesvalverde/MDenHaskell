module ConectividadGrafosSpec where

import Conjuntos
import Relaciones
import RelacionesHomogeneas
import Funciones
import GrafoConListaDeAristas
import EjemplosGrafos
import GeneradorGrafos
import DefinicionesYPropiedades
import Morfismos
import ConectividadGrafos

import Test.Hspec       
import Test.QuickCheck
import Data.List
import Data.Maybe

ejemplosConectividadGrafos :: Spec
ejemplosConectividadGrafos =
  describe "ConectividadGrafos" $ do
  describe "esCamino" $ do
    it "e1" $
      esCamino (grafoCiclo 5) [1,2,3,4,5,1] `shouldBe`
      True
    it "e2" $
      esCamino (grafoCiclo 5) [1,2,4,5,3,1] `shouldBe`
      False
    it "e3" $
      esCamino grafoThomson [1,2,3]         `shouldBe`
      False
    it "e4" $
      esCamino grafoThomson [1,4,2,5,3,6]   `shouldBe`
      True

  describe "aristasCamino" $ do
    it "e1" $
      aristasCamino [1,2,3,4,5,1] `shouldBe`
      [(1,2),(2,3),(3,4),(4,5),(1,5)]
    it "e2" $
      aristasCamino [1,2,4,5,3,1] `shouldBe`
      [(1,2),(2,4),(4,5),(3,5),(1,3)]
    it "e3" $
      aristasCamino [1,2,3] `shouldBe`
      [(1,2),(2,3)]
    it "e4" $
      aristasCamino [1,4,2,5,3,6] `shouldBe`
      [(1,4),(2,4),(2,5),(3,5),(3,6)]

  describe "verticesCamino" $ do
    it "e1" $
      verticesCamino [1,2,3,4,5,1] `shouldBe`
      [1,2,3,4,5]
    it "e2" $
      verticesCamino [1,2,4,5,3,1] `shouldBe`
      [1,2,4,5,3]
    it "e3" $
      verticesCamino [1,2,3] `shouldBe`
      [1,2,3]
    it "e4" $
      verticesCamino ([]::[Int]) `shouldBe`
      []

  describe "esRecorrido" $ do
    it "e1" $
      esRecorrido (completo 5) [1,2,3] `shouldBe`
      True
    it "e2" $
      esRecorrido (completo 5) [1,2,3,2] `shouldBe`
      False

  describe "esCaminoSimple" $ do
    it "e1" $
      esCaminoSimple (completo 5) [1,2,3]      ==  True
    it "e2" $
      esCaminoSimple (completo 5) [1,2,3,1]    ==  True
    it "e3" $
      esCaminoSimple (completo 5) [1,2,3,4,2]  ==  False
    it "e4" $
      esRecorrido (completo 5) [1,2,3,4,2]     ==  True

  describe "" $ do
    it "e1" $
      longitudCamino [1,2,3,4]   == 3
    it "e2" $
      longitudCamino ['a'..'z']  == 25
    it "e3" $
      longitudCamino [2,4..10]   == 4            

  describe "todosCaminosBP" $ do
    it "e1" $
      todosCaminosBP (grafoCiclo 7) 1 6 `shouldBe`
      [[1,2,3,4,5,6],[1,7,6]]
    it "e2" $  
      todosCaminosBP (grafoRueda 7) 2 5 `shouldBe`
      [[2,1,5],[2,3,1,5],[2,3,4,1,5],[2,7,6,1,5],[2,7,1,5],[2,1,4,5],
       [2,3,1,4,5],[2,7,6,1,4,5],[2,7,1,4,5],[2,1,3,4,5],[2,7,6,1,3,4,5],
       [2,7,1,3,4,5],[2,3,4,5],[2,1,6,5],[2,3,1,6,5],[2,3,4,1,6,5],
       [2,7,1,6,5],[2,1,7,6,5],[2,3,1,7,6,5],[2,3,4,1,7,6,5],[2,7,6,5]]
    it "e3" $  
      todosCaminosBP (creaGrafo [1..4] [(1,2),(2,3)]) 1 4 `shouldBe`
      []

  describe "todosCaminosBA" $ do
    it "e1" $
      todosCaminosBA (grafoCiclo 7) 1 6 `shouldBe`
      [[1,7,6],[1,2,3,4,5,6]]
    it "e2" $
      todosCaminosBA (grafoRueda 7) 2 5 `shouldBe`
      [[2,1,5],[2,3,1,5],[2,7,1,5],[2,1,4,5],[2,3,4,5],[2,1,6,5],[2,7,6,5],
       [2,3,4,1,5],[2,7,6,1,5],[2,3,1,4,5],[2,7,1,4,5],[2,1,3,4,5],
       [2,3,1,6,5],[2,7,1,6,5],[2,1,7,6,5],[2,7,6,1,4,5],[2,7,1,3,4,5],
       [2,3,4,1,6,5],[2,3,1,7,6,5],[2,7,6,1,3,4,5],[2,3,4,1,7,6,5]]
    it "e3" $  
      todosCaminosBA (creaGrafo [1..4] [(1,2),(2,3)]) 1 4 `shouldBe`
      []
    it "p1" $
      property $ prop_todosCaminosBA


  describe "estanConectados" $ do
    it "e1" $  
      estanConectados (grafoCiclo 7) 1 6                    ==  True
    it "e2" $  
      estanConectados (creaGrafo [1..4] [(1,2),(2,3)]) 1 4  ==  False
    it "e3" $  
      estanConectados grafoNulo 1 4                         ==  False

  describe "distancia" $ do
    it "e3" $  
      distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 1  ==  Just 0
    it "e2" $  
      distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 2  ==  Just 1
    it "e3" $  
      distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 3  ==  Just 2
    it "e4" $  
      distancia (creaGrafo [1..4] [(1,2),(2,3)]) 1 4  ==  Nothing

  describe "esGeodesica" $ do
    it "e1" $  
      esGeodesica (grafoCiclo 7) [1,7,6]                  == True            
    it "e2" $  
      esGeodesica (grafoCiclo 7) [1,2,3,4,5,6]            == False
    it "e3" $  
      esGeodesica (grafoRueda 7) [2,1,5]                  == True
    it "e4" $  
      esGeodesica (creaGrafo [1..4] [(1,2),(2,3)]) [1,4]  == False
    it "e5" $  
      esGeodesica grafoNulo [1,4]                         == False

  describe "esCerrado" $ do
    it "e1" $  
      esCerrado (grafoCiclo 5) [1,2,3,4,5,1]  ==  True
    it "e2" $  
      esCerrado (grafoCiclo 5) [1,2,4,5,3,1]  ==  False
    it "e3" $  
      esCerrado grafoThomson [1,4,2,5,3,6]    ==  False
    it "e4" $  
      esCerrado grafoThomson [1,4,2,5,3,6,1]  ==  True
    it "e5" $  
      esCerrado grafoNulo [1,2,1]             ==  False

  describe "esCircuito" $ do
    it "e1" $  
      esCircuito (grafoCiclo 5) [1,2,3,4,5,1]    ==  True
    it "e2" $  
      esCircuito (grafoCiclo 3) [1,2,3,1,2,4,1]  ==  False
    it "e3" $  
      esCircuito grafoThomson [1,4,2,5,3,6]      ==  False
    it "e4" $  
      esCircuito grafoThomson [1,4,2,5,3,6,1]    ==  True
    it "e5" $  
      esCircuito grafoNulo [1,2,1]               ==  False     

  describe "esCiclo" $ do
    it "e1" $  
      esCiclo (grafoCiclo 5) [1,2,3,4,5,1]    ==  True
    it "e2" $  
      esCiclo (grafoCiclo 3) [1,2,3,1,2,4,1]  ==  False
    it "e3" $  
      esCiclo grafoThomson [1,4,2,5,3,6]      ==  False
    it "e4" $  
      esCiclo grafoThomson [1,4,2,5,3,6,1]    ==  True
    it "e5" $  
      esCiclo grafoNulo [1,2,1]               ==  False

  describe "estarConectadosCamino" $ do
    it "e1" $
      estarConectadosCamino (creaGrafo [1..4] [(1,2),(2,4)]) `shouldBe`
      [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4)]
    it "p1" $
      property $ prop_conectadosRelEqui

  describe "componentesConexas" $ do
    it "e1" $
      componentesConexas grafoPetersen `shouldBe`
      [[1,2,3,4,5,6,7,8,9,10]]
    it "e2" $
      componentesConexas (creaGrafo [1..5] [(1,2),(2,3)]) `shouldBe`
      [[1,2,3],[4],[5]]
    it "e3" $
      componentesConexas (creaGrafo [1..5] [(1,2),(2,3),(4,5)]) `shouldBe`
      [[1,2,3],[4,5]]
    it "e4" $
      componentesConexas (grafoNulo :: Grafo Int) `shouldBe`
      []

  describe "esConexo" $ do
    it "e1" $
      esConexo (completo 5)                      == True
    it "e2" $
      esConexo (creaGrafo [1..5] [(1,2),(2,3)])  == False
    it "e3" $
      esConexo (creaGrafo [1..3] [(1,2),(2,3)])  == True
    it "e4" $
      esConexo (grafoNulo :: Grafo Int)          == False    
    it "p1" $
      property $ prop_caracterizaGrafoConexo

  describe "excentricidad" $ do
    it "e1" $
      excentricidad (grafoCiclo 8) 5               ==  Just 4
    it "e2" $
      excentricidad (grafoRueda 7) 4               ==  Just 2
    it "e3" $
      excentricidad (grafoRueda 7) 1               ==  Just 1
    it "e4" $
      excentricidad grafoPetersen  6               ==  Just 2
    it "e5" $
      excentricidad (creaGrafo [1,2,3] [(1,2)]) 3  ==  Nothing
    it "e6" $
      excentricidad grafoNulo 3                    ==  Nothing
      
  describe "diametro" $ do     
    it "e1" $
      diametro (grafoCiclo 8)              ==  Just 4
    it "e2" $
      diametro (grafoRueda 7)              ==  Just 2
    it "e3" $
      diametro grafoPetersen               ==  Just 2
    it "e4" $
      diametro (creaGrafo [1,2,3] [(1,2),(2,3)])   ==  Just 2
    it "e5" $
      diametro (grafoNulo :: Grafo Int)    ==  Nothing       

  describe "radio" $ do
    it "e1" $
      radio (grafoCiclo 8)              ==  Just 4
    it "e2" $
      radio (grafoRueda 7)              ==  Just 1
    it "e3" $
      radio grafoPetersen               ==  Just 2
    it "e4" $
      radio (creaGrafo [1,2,3] [(1,2),(2,3)])  ==  Just 1
    it "e5" $
      radio (grafoNulo :: Grafo Int)    ==  Nothing

  describe "centro" $ do
    it "e1" $
      centro (grafoEstrella 5)  ==  [1]
    it "e2" $
      centro (grafoCiclo 4)     ==  [1,2,3,4]
    it "e3" $
      centro grafoPetersen      ==  [1,2,3,4,5,6,7,8,9,10]
    it "e4" $
      centro (grafoRueda 5)     ==  [1]
    it "e5" $
      centro (grafoNulo :: Grafo Int) ==  []

verificaConectividadGrafos :: IO ()
verificaConectividadGrafos = 
  hspec ejemplosConectividadGrafos
    
