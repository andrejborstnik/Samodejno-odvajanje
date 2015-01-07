{-|
Module      : Limits
Description :  Levi in desni odvodi (neodvedljivih) funkcij
Copyright   : (c) Andrej Borštnik, Barbara Bajcer, 2015
Maintainer  : barbara.bajcer@gmail.com, andrej-borstnik@hotmail.com
Stability   : experimental 

-}

module Limits.Limits
( D(..)
, constD
, idD
, sqr
)
where

import Control.Applicative

instance Num b => Num (a -> b) where
	fromInteger = pure . fromInteger
	negate = fmap negate
	(+) = liftA2 (+)
	(*) = liftA2 (*)
	abs = fmap abs
	signum = fmap signum

instance Fractional b => Fractional (a -> b) where 
	fromRational = pure . fromRational
	recip = fmap recip	

instance Floating b => Floating (a -> b) where
	pi = pure pi
	sqrt = fmap sqrt
	exp = fmap exp
	log = fmap log
	sin = fmap sin
	cos = fmap cos
	asin = fmap asin
	atan = fmap atan
	acos = fmap acos
	sinh = fmap sinh 
	cosh = fmap cosh
	asinh = fmap asinh
	acosh = fmap acosh
	atanh = fmap atanh

-- |Struktura D predstavlja točko ter levi in desni odvod v njej.
data D a = D a a a

instance (Show a) => Show (D a) where
	show (D a b c) = "D " ++ show a ++ " " ++ show b ++ " " ++ show c
 		--"V točki " ++ show a ++ " je leva limita funkcije enaka " ++ show b ++ " in desna " ++ show c ++ "."

-- |Izračuna vrednost in leve/desne odvode funkcije f v točki a, kjer je f' odvod f.
infix 0 ><
(><) :: Num a => (a -> a) -> (a -> a) -> (D a -> D a)
(f >< f') (D a al' ar') = D (f a) (al' * f' a) (ar' * f' a)

-- |@constD@ preslika numeral Num v točko (konstanto).
constD :: Num a => a -> D a
constD x = D x 0 0

-- |Identična funkcija.
idD :: Num a => a -> D a
idD x = D x 1 1

-- |Funkcija kvadriranja.
sqr :: Num a => a -> a
sqr a = a * a


instance (Num a, Eq a) => Num (D a) where
	fromInteger = constD . fromInteger
	D a al' ar' + D b bl' br' = D (a + b) (al' + bl') (ar' + br')
	(D x xl' xr') * (D y yl' yr') = D (x * y) (xl' * y + yl' * x) (xr' * y + yr' * x)
	negate = negate >< -1
	signum = signum >< 0
	abs x@(D 0 xl' xr') = D 0 (- abs xl') (abs xr') -- needed, because signum can (theoretically) have any value between -1 and 1 in 0
	abs x = (abs >< signum) x

		-- abs x@(D y xl' xr') = 
		-- if (y == fromInteger 0) then D 0 (- abs xl') (abs xr') 
		-- else (abs >< signum) x
	
instance (Fractional a, Eq a) => Fractional (D a) where
	fromRational = constD . fromRational
	recip = recip >< -sqr recip

instance (Floating a, Eq a) => Floating (D a) where
	pi = constD pi
	exp = exp >< exp
	log = log >< recip
	sqrt = sqrt >< recip (2 * sqrt)
	sin = sin >< cos
	cos = cos >< - sin
	asin = asin >< recip (sqrt (1 - sqr))
	acos = acos >< - recip (sqrt (1 - sqr))
	atan = atan >< recip (1 + sqr)
	sinh = sinh >< cosh
	cosh = cosh >< sinh
	asinh = asinh >< recip (sqrt (1 + sqr))
	acosh = acosh >< recip (sqrt (- 1 + sqr))
	atanh = asin >< recip (1 - sqr)

instance Eq a => Eq (D a) where
	(==) (D a _ _) (D b _ _) = (==) a b
	
instance Ord a => Ord (D a) where
	min x@(D a al' ar') y@(D b bl' br')
		| a < b || (al' > bl' && ar' < br') = x
		| b < a || (bl' > al' && ar' > br') = y
		| al' > bl' = D a al' br'
		| otherwise = D a bl' ar'
		
	max x@(D a al' ar') y@(D b bl' br')
		| a < b || (al' < bl' && ar' > br') = x
		| b < a || (bl' < al' && ar' < br') = y
		| al' < bl' = D a al' br'
		| otherwise = D a bl' ar'
			
	-- (<=) (D a _ _) (D b _ _) = (<=) a b
	
f1 :: Floating a => a -> a
f1 z = sqrt ((sqr z) * 3 * sin z)

f2 :: (Ord a, Floating a) => a -> a
f2 z = min (min (sin (2*z)) 0) (min (-z) 0)

f3 :: Floating a => a -> a
f3 z = abs z

f4 :: (Floating a, Ord a) => a -> a
f4 z = min (sin z) 0