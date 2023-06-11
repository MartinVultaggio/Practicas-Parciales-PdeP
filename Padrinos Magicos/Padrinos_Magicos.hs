import Text.Show.Functions ()
import Data.List (intersect,concat,zipWith)

--A.

data Chico = UnChico {
nombre :: String,
edad :: Int,
habilidades :: [Habilidad],
deseos :: [Deseo]
} deriving Show

type Habilidad = String
type Deseo = Chico -> Chico

--A.1.
aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = unChico {habilidades = unasHabilidades ++ habilidades unChico}

--serGrosoEnNeedForSpeed :: Deseo
--serGrosoEnNeedForSpeed unChico = unChico {habilidades = zipWith concat (repeat "Jugar Need for Speed ") (iterate (+1) 1) ++ habilidades unChico}

modificarEdad :: (Int->Int) -> Chico -> Chico
modificarEdad unaFuncion unChico = unChico {edad = unaFuncion . edad $ unChico}

serMayor :: Deseo
serMayor = modificarEdad ((+18).(*0))

--A.2.
type Padrino = Chico -> Chico

wanda :: Padrino
wanda unChico = modificarEdad (+1) . (head . deseos $ unChico) $ unChico

cosmo :: Padrino
cosmo unChico = modificarEdad (flip div 2) unChico

muffinMagico :: Padrino
muffinMagico unChico = foldl (\acc deseo -> deseo acc) unChico (deseos unChico)


--B.

--B.1.
type Condicion = Chico -> Bool

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad habilidad = elem habilidad . habilidades

esSuperMaduro :: Condicion
esSuperMaduro unChico = ((>=18) . edad $ unChico) && (tieneHabilidad "Manejar" unChico)

--B.2.
noEsTimmy :: Condicion
noEsTimmy = (/= "Timmy") . nombre

data Chica = UnaChica {
nombreChica :: String,
condicion :: Condicion
}

--asumo que siempre hay pretendientes
quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica pretendientes
	|any (condicion unaChica) pretendientes = head . filter (condicion unaChica) $ pretendientes
	|otherwise = last pretendientes

chicaEjemplo :: Chica
chicaEjemplo = UnaChica{
	nombreChica = "Chica de Ejemplo",
	condicion = tieneHabilidad "Cocinar"
}


--C.

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["Enamorar", "Matar", "Dominar el Mundo"]

tieneDeseosProhibidos :: Chico -> Bool
tieneDeseosProhibidos = (>0) . length . intersect habilidadesProhibidas . take 5 . habilidades . muffinMagico

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules unChico = map nombre . filter tieneDeseosProhibidos $ unChico



--D.
{-
Use composicion de funciones en:
_ serGrosoEnNeedForSpeed: para convertir los numeros de c/juego a string
_ modificarEdad: para aplicar una funcion a la dead de un chico
_ serMayor: para borrar la edad actual e intercambiarla por 18 años de edad
_ wanda: para sumarle 1 a la edad de un chico y cumplir su primer deseo
_ tieneHabilidad: para saber si la habilidad ingresada esta en la lista de habilidades del chico
_ esSuperMaduro: para saber si el chico tiene 18 o mas años
_ noEsTimmy: para saber si el nombre del chico es distinto de timmy
_ tieneDeseosProhibidos: en este ejercicio lo use 5 veces, jaja, cuestion
	1. para buscar las habilidades actualizadas del chico, o sea tras cumplir todos sus deseos
	2. para quedarse con las primeras 5 habilidades
	3. para saber cuales de estas habilidades estan prohibidas
	4. para saber cuantas habilidades prohibidas tiene
	5. para saber si tiene mas de 0, o sea si tiene habilidades prohibidas
_ infractoresDeDaRules: para quedarse con lo nombres de los infractores

Creé varias funciones de orden superior:
_ serGrosoEnNeedForSpeed: para juntar 2 listas en base a una funcion (concat) y para que toda la lista de numeros sea de strings 
_ muffinMagico: aplicarle todos los deseos al chico
_ quienConquistaA: para saber si habia un digno pretendiente si quiera
_ tieneDeseosProhibidos: para aplicarle todos los deseos a todos los chicos
_ infractoresDeDaRules: filtreé listas

Una funcion utiliza listas infinitas, serGrosoEnNeedForSpeed, puesto que no se sabe cuantos juegos va a haber en el futuro. Esta funcion no funcionaria, puesto que nunca terminaria de agregarle habilidades al chico, el chico estaria constantemente maestrando juegos y nunca terminaria.
-}