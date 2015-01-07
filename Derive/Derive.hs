{-|
Module      : Derive
Description : Samodejno odvajanje
Copyright   : (c) Andrej Borštnik, Barbara Bajcer, 2015
Maintainer  : andrej-borstnik@hotmail.com, barbara.bajcer@gmail.com
Stability   : experimental 

Samodejno računanje vrednosti funkcije in njenih odvodov v dani točki.
-}
module Derive
( D(..)
, constD
, idD
, (+&)
, (*&)
, sqr
, (><) 
, first_n
, nth
)
where


import Control.Applicative

-- |Vrednost funkcije in njene odvode v neki točki bomo predstavili s tipom @D@.
data D a = D a (Maybe (D a))

-- |
instance Num b => Num (a -> b) where
	fromInteger = pure . fromInteger
	negate = fmap negate
	(+) = liftA2 (+)
	(*) = liftA2 (*)
	abs = fmap abs
	signum = fmap signum

-- |
instance Fractional b => Fractional (a -> b) where 
	fromRational = pure . fromRational
	recip = fmap recip	

-- |
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

-- |
instance Show a => Show (D a) where
	show (D a b) = show a ++ ", " ++ show1 b where
		show1 Nothing = show 0
		show1 (Just c) = show c

-- |@constD@ je konstanta.
constD :: Num a => a -> D a
constD x = D x Nothing

-- |@idD@ je identiteta.
idD :: Num a => a -> D a
idD x = D x (Just 1)

-- |Definiramo seštevanje za tip @Maybe@.
infixl 6 +&
(+&) :: Num a => Maybe a -> Maybe a -> Maybe a
Nothing +& b = b
a +& Nothing = a
Just a +& Just b = Just (a + b)

-- |Definiramo množenje za tip @Maybe@.
infixl 7 *&
(*&) :: Num a => Maybe a -> a -> Maybe a
Nothing *& _ = Nothing
Just a *& b = Just (a * b)

-- |Definiramo uporabo funkcij na tipu @D@.
infix 0 ><
(><) :: Num a => (a -> a) -> (D a -> D a) -> (D a -> D a)
(f >< f') a@(D a0 a') = D (f a0) (a' *& f' a)

-- |
sqr :: Num a => a -> a
sqr x = x * x

-- |
instance Num a => Num (D a) where
	fromInteger = constD . fromInteger
	D a a' + D b b' = D (a + b) (a' +& b')
	x@(D x0 x') * y@(D y0 y') = D (x0 * y0) (x' *& y +& y' *& x)
	negate = negate >< -1
	signum = signum >< 0
	abs = abs >< signum
	
-- |
instance Fractional x => Fractional(D x) where
	fromRational = constD . fromRational
	recip = recip >< -sqr recip

-- |
instance Floating x => Floating (D x) where
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
	
-- |
instance Eq a => Eq (D a) where
	(==) (D a _) (D b _) = (==) a b
	
-- |
instance Ord a => Ord (D a) where
	(<=) (D a _) (D b _) = (<=) a b

-- |Funkcija @first_n@ nam izpiše vrednost funkcije f ter prvih n odvodov v neki točki x.
first_n :: Floating a => (D a -> D a) -> a -> Int -> [a]
first_n f x n = (sez pomo n) where
	pomo = f (D x (Just 1))
--	sez :: Num b => D a -> b -> [a]
 	sez (D y _) 0 = y : []
	sez (D y Nothing) _ = 0 : []
	sez (D y (Just b)) m = y : sez b (m - 1)

-- |Funkcija @nth@ nam izpiše n-ti odvod funkcije f v točki x.
nth :: Floating a => (D a -> D a) -> a -> Int -> a
nth f x n = r where
	s = first_n f x n
	l = length s
	r = if n > l then 0 else s!!n
	
	
	
	

f1 :: Floating a => a -> a
f1 z = sqrt ((sqr z) * 3 * sin z)

f2 :: Floating a => a -> a
f2 z = sqr z

f3 :: Floating a => a -> a
f3 z = abs z

f4 :: Floating a => a -> a
f4 z = z