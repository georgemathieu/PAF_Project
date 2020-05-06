module Modele where

import qualified Data.Map.Strict as M

import qualified System.Random as R

import Carte
import Environnement
import Keyboard

data Modele = Cont {carte :: Carte, -- carte actuelle
                    envi :: Envi, -- environnement actuel
                    gene :: R.StdGen, -- generateur aleatoire
                    log :: String, -- journal
                    keyboard :: Keyboard -- l'etat du clavier
                    }


bouge :: Modele -> Entite -> Coord -> Modele
bouge (Cont carte env seed log keyboard) e coord = 
  (Cont carte (bouge_id (entite_get_id e) coord env) seed (log ++ "entite deplacee") keyboard)
