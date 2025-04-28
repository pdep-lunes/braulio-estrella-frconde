module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
nombre :: String, 
poderBasico :: String, 
superPoder :: String, 
superPoderActivo :: Bool, 
cantidadDeVida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas" True 4800
pamela :: Personaje
pamela = UnPersonaje "Pamela" "Lluvia de Tuercas sanadoras" "Torreta curativa" False 9600

obtenerCantidadDeVida :: Personaje -> Int
obtenerCantidadDeVida = cantidadDeVida

bolaEspinosa :: Personaje -> Int
bolaEspinosa unPersonaje = cantidadDeVida unPersonaje - 1000