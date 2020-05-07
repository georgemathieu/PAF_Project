module Modele where

import qualified Data.Map.Strict as M

import qualified System.Random as R

import Data.Random as DR
import Data.Random.Shuffle.Weighted as RSW
import Control.Monad.State.Lazy as MSL

import Carte
import Environnement
import Keyboard

data Modele = Cont {carte :: Carte, -- carte actuelle
                    envi :: Envi, -- environnement actuel
                    gene :: R.StdGen, -- generateur aleatoire
                    log :: String, -- journal
                    keyboard :: Keyboard -- l'etat du clavier
                    } deriving (Show)

data Ordre = N | S | E | O | U | R deriving (Show)
-- resp. Nord, Sud, Est, Ouest, Utiliser, Rien faire

-- deplace une entite
bouge :: Modele -> Entite -> Coord -> Modele
bouge (Cont carte env seed log keyboard) e coord = 
  (Cont carte (bouge_id (entite_get_id e) coord env) seed (log ++ "entite deplacee") keyboard)


-- decide de l'action a faire suivant une liste ponderee
decide :: [(Int, Ordre)] -> Modele -> Entite -> Ordre
decide listeOrdre modele@(Cont c env seed log keyboard) entite = 
  appliquerOrdre ((MSL.evalState (DR.runRVar (RSW.weightedSample 1 listeOrdre) DR.StdRandom) seed) !! 0) entite modele


appliquerOrdre :: Ordre -> Entite -> Modele -> Ordre
appliquerOrdre o e m = o
