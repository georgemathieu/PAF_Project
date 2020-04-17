module Carte where

import qualified Data.Map.Strict as M

-- direction d'une porte, NORD/SUD ou EST/OUEST
data PorteDirection = NS | EO deriving (Eq)

-- statut d'une porte, ouverte ou fermée
data StatutPorte = Ouverte | Fermee deriving (Eq)

-- Contenu possible d'une case
data Case = Normal
   | Porte PorteDirection StatutPorte
   | Mur
   | Entree
   | Sortie
   | Other
   deriving (Eq)

data Coord = C {cx :: Int, cy :: Int} deriving (Eq)

-- ordre de la carte, début au nord-ouest, fin sud-est
instance Ord Coord where
  (C cx1 cy1) <= (C cx2 cy2) 
    | cy1 < cy2 = True
    | cy1 > cy2 = False
    | cy1 == cy2 = cx1 < cx2

data Carte = Carte {cartel :: Int, -- largeur
                    carteh :: Int, -- hauteur
                    carteContenu :: (M.Map Coord Case)} -- contenu de la carte

-- fonction qui gère l'affichage, on reste à qqch de simple pour le moment
foldCarteAux :: String -> Case -> String
foldCarteAux s c 
  | c == Normal = s ++ " "
  | c == (Porte NS Ouverte) = s ++ " "
  | c == (Porte EO Ouverte) = s ++ " "
  | c == (Porte NS Fermee) = s ++ "-"
  | c == (Porte EO Fermee) = s ++ "|"
  | c == Mur = s ++ "X"
  | c == Entree = s ++ "E"
  | c == Sortie = s ++ "S"
  | c == Other = s ++ ""

-- carte complète avec \n selon la largeur
cartePrinter :: String -> Int -> String
cartePrinter s l = cartePrinterAux s l 0 "" where
  cartePrinterAux :: String -> Int -> Int -> String -> String
  cartePrinterAux s l i res 
    | i >= (length s) = res
    | i `mod` l == 0 && (length res) /= 0 = cartePrinterAux s l (i+1) (res ++ "\n" ++ [s!!i])
    | otherwise = cartePrinterAux s l (i+1) (res ++ [s!!i])


instance Show Carte where
    show (Carte l h content) = 
      cartePrinter (foldl foldCarteAux "" content) l

charToCase :: Char -> Case
charToCase c
  | c == ' ' = Normal
  | c == '-' = (Porte NS Fermee)
  | c == '|' = (Porte EO Fermee)
  | c == 'X' = Mur
  | c == 'E' = Entree
  | c == 'S' = Sortie
  | otherwise = Other

loadCarte :: String -> Carte
loadCarte s = readMapInput 0 0 0 0 s M.empty where
      readMapInput :: Int -> Int -> Int -> Int -> String -> M.Map Coord Case -> Carte
      readMapInput x y i l s map -- largeur, hauteur, index, largeur conserve, carte, map de la carte
        | length s <= i = (Carte l y map)
        | s!!i == '\n' = readMapInput 0 (y+1) (i+1) x s map
        | otherwise = readMapInput (x+1) y (i+1) l s (M.insert (C x y) (charToCase (s!!i)) map)

instance Read Carte where
    readsPrec _ s = readMapInput 0 0 0 s M.empty where
      readMapInput :: Int -> Int -> Int -> String -> M.Map Coord Case -> [(Carte, String)]
      readMapInput x y i s map 
        | s!!i == '\n' = readMapInput 0 (y+1) (i+1) s (M.insert (C x y) (charToCase (s!!i)) map)
        | length s == i = [((Carte x y map),"")]
        | otherwise = readMapInput (x+1) y (i+1) s (M.insert (C x y) (charToCase (s!!i)) map)
