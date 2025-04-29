module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
nombre :: String, 
poderBasico :: (Personaje -> Personaje), 
superPoder :: (Personaje -> Personaje), 
superPoderActivo :: Bool, 
cantidadDeVida :: Int
} deriving Show

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

torretaCurativa :: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje {superPoderActivo = True, cantidadDeVida = cantidadDeVida unPersonaje * 2 }


atacarPoderEspecial :: Personaje -> Personaje -> Personaje
atacarPoderEspecial personajeAtacante personajeAtacado 
    | superPoderActivo personajeAtacante = (poderBasico personajeAtacante).(superPoder personajeAtacante) $ personajeAtacado
    | otherwise = personajeAtacado

equipo1 = [pamela, espina]
equipo2 = [pamela, espina, juancito, pedrito]

menosDe800DeVida :: [Personaje] -> [Personaje]
menosDe800DeVida equipo = filter ((< 800).cantidadDeVida) equipo

enLasUltimas :: [Personaje] -> [String]
enLasUltimas equipo = map nombre (menosDe800DeVida equipo)


espina :: Personaje
espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 400
pamela :: Personaje
pamela = UnPersonaje "Pamela" (lluviaDeTuercas "Sanadoras") torretaCurativa False 9600
juancito :: Personaje
juancito = UnPersonaje "Juancito" bolaEspinosa torretaCurativa False 300
pedrito :: Personaje
pedrito = UnPersonaje "Pedrio" bolaEspinosa torretaCurativa False 4000

