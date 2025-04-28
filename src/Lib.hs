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
obtenerNombre :: Personaje -> String
obtenerNombre = nombre

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = unPersonaje {cantidadDeVida = max (obtenerCantidadDeVida unPersonaje - 1000) 0 } 

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoDePoder unPersonaje
    | tipoDePoder == "Sanadoras" = unPersonaje {cantidadDeVida = obtenerCantidadDeVida unPersonaje + 800}
    | tipoDePoder == "Daninas" = unPersonaje {cantidadDeVida = div (obtenerCantidadDeVida unPersonaje) 2}
    | otherwise = unPersonaje
granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio unPersonaje
    | radio > 3 && obtenerCantidadDeVida unPersonaje < 800 = unPersonaje {superPoderActivo = False, cantidadDeVida = 0}
    | radio > 3 = unPersonaje {nombre = obtenerNombre unPersonaje ++ "Espina estuvo aqui"}
    | otherwise = bolaEspinosa unPersonaje