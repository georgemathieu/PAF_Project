module Environnement where

import qualified Data.Map.Strict as M

import Carte

-- environnement : position des entites
data Envi = Envi {contenu_envi :: M.Map Coord [Entite]}

-- a completer avec d'autres entites pour rajouter du contenu
data Entite = Vache {idEn :: Int, pv :: Int}
              | Joueur {idEn :: Int, pv :: Int}
              deriving (Eq)


-- Determine si une case est franchissable
-- True si la case en franchissable, False sinon
franchissable_env :: Coord -> Envi -> Bool
franchissable_env coord (Envi contenu_envi) = case M.lookup coord contenu_envi of
  Just listeEntite -> isListeFranchissable listeEntite
  Nothing -> error "coordonnes non existantes"
  where
    isListeFranchissable :: [Entite] -> Bool
    isListeFranchissable [] = True
    isListeFranchissable ((Joueur idEn pv):es) = False
    isListeFranchissable ((Vache idEn pv):es) = False
    -- si on ajoute des entite traversables (objets), c'est de la forme
    -- isListeFranchissable ((nomEntite attributd):es) = isListeFranchissable es


-- trouve une entite et ses coordonnes a partir de son identifiant
trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id idEn (Envi contenu_envi) = M.foldlWithKey' trouve_id_aux Nothing contenu_envi where
  trouve_id_aux :: Maybe (Coord, Entite) -> Coord -> [Entite] -> Maybe (Coord, Entite)
  trouve_id_aux (Just (c, e)) _ _ = Just (c, e)
  trouve_id_aux _ _ [] = Nothing
  trouve_id_aux Nothing coord (e:es) = if idEn == (entite_get_id e) then Just (coord, e)
                                          else trouve_id_aux Nothing coord es


-- enleve une entite de l'environnement a partir de son identifiant
rm_env_id :: Int -> Envi -> Envi
rm_env_id idEn env@(Envi contenu_envi) = case trouve_id idEn env of 
  Just (coord, entite) -> case M.lookup coord contenu_envi of
    Just listeEntite -> (Envi (M.insert coord (removeElement entite listeEntite) contenu_envi))
    Nothing -> error "coordonnee non existante (???)"
  Nothing -> error "entite non existante"


-- deplace une entite a partir de son identifiant
bouge_id :: Int -> Coord -> Envi -> Envi
bouge_id idEn newCoord env@(Envi contenu_envi) = case trouve_id idEn env of 
  Just (coord, entite) -> do
    let (Envi newContenu_envi) = rm_env_id idEn env
    (Envi (M.insert newCoord [entite] newContenu_envi))
  Nothing -> error "entite non existante"

-- recupere l'id d'une entite
entite_get_id :: Entite -> Int
entite_get_id (Joueur idEn _) = idEn
entite_get_id (Vache idEn _) = idEn


-- retire un element d'une liste
removeElement :: (Eq a) => a -> [a] -> [a]
removeElement _ [] = []
removeElement e (x:xs)
  | e == x = removeElement e xs
  | otherwise = x : removeElement e xs



