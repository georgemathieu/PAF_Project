module ModeleTest where

import Test.Hspec

import Carte
import Environnement
import Modele

import qualified Data.Map.Strict as M
import System.Random as R
import Data.Set (Set)
import qualified Data.Set as S

modeleTest = do 
    describe "Modele:decide" $ do
        it "first decide test" $ do
            let x = (Envi (M.fromList []))
            let j1 = (Joueur 1 10)
            let env = ajout_entite j1 (C 1 1) x
            x <- readFile "cartes/carte.txt"
            let carte = read x :: Carte
            gen <- R.getStdGen
            let keyboard = S.fromList []
            let modele = (Cont carte env gen "" keyboard)
            let listeAction = [(2,N),(4,S),(1,E),(1,O)]
            let modele' = decide listeAction modele j1
            (show $ modele') `shouldBe` "Just (C {cx = 1, cy = 1},Joueur {idEn = 1, pv = 10})"

