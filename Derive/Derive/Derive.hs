{-|
Module      : Derive
Description : Samodejno odvajanje
Copyright   : (c) Andrej Borštnik, Barbara Bajcer, 2015
Maintainer  : andrej-borstnik@hotmail.com, barbara.bajcer@gmail.com
Stability   : experimental 

Samodejno računanje vrednosti funkcije in njenih odvodov v dani točki.
-}
module Derive.Derive
( D(..)
, constD
, idD
, sqr
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

infixl 6 +&
(+&) :: Num a => Maybe a -> Maybe a -> Maybe a
Nothing +& b = b
a +& Nothing = a
Just a +& Just b = Just (a + b)

infixl 7 *&
(*&) :: Num a => Maybe a -> a -> Maybe a
Nothing *& _ = Nothing
Just a *& b = Just (a * b)

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

f5 :: Floating a => a -> a
f5 z = exp z / z + 2 * (sqr z) * (sin z)
	
f6 :: Floating a => a -> a
f6 z = (sqr z) * (sqr z) * (sqr z)

f7 :: Floating a => a -> a
f7 z = sin (sqr z)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


-- data DD a b = DD b (LMap a b)

-- instance Functor (DD a) where 
	-- fmap g = g >< d g

-- class Functor f => Applicative f where
	-- pure :: a -> f a
	-- (<*>) :: f (a -> b) -> f a -> f b

-- class Monodial f where
	-- unit :: f ()
	-- zipp :: f a -> f b -> f (a, b)
	
-- (><) :: (Vector s u, Vector s v, Vector s w) => (v -> w) -> (v -> (LMap v w)) -> (DD u v) -> (DD u w)
-- g >< dg (D fx dfx) = D (g fx) (dg fx) compose dfx
-- class AdditiveGroup v where
	-- zero :: v
	-- (+++) :: v -> v -> v
	-- negateg :: v -> v

-- class AdditiveGroup v => Vector s v where
	-- (***) :: s -> v -> v
	
-- class Vector s v => InnerSpace s v where
	-- (<*>) :: v -> v -> s
	
-- instance AdditiveGroup v => AdditiveGroup (a -> v) where
	-- zero = pure zero
	-- (+++) = liftA2 (+++)
	-- negateg = fmap negate

-- instance Vector s v => Vector s (a -> v) where
	-- (***) s = fmap (s ***)

-- newtype LMap u v = LMap (u -> v) deriving (AdditiveGroup, Vector)

-- linear :: (Vector a u, Vector s u) => (u -> v) -> (LMap u v)
-- lapply :: (Vector s u, Vector s v) => (LMap u v) -> (u -> v)
-- idL :: (Vector s u) => LMap u u
-- compose :: (Vector s u, Vector s v) => (LMap v w) -> (LMap u v) -> (LMap u w)
-- join :: (Vector s u, Vector s v, Vector s w) => (LMap u w) -> (LMap v w) -> (LMap (u,v) w)
-- zipp :: (LMap w u) -> (LMap w v) -> (LMap w (u,v))

-- instance Monodial ((->) a) where
	-- unit = const ()
	-- f zipp g = \x -> (f x, g x)

-- instance Applicative ((->) a) where
	-- pure a = fmap (const a) unit
	-- fs <*> xs = fmap app (fs zipp sx) where
		-- app :: (a -> b, a) -> b
		-- app (f, x) = f x

-- unit = D () 0

-- d :: (Vector s u, Vector s v) => (u -> v) -> (u -> (LMap u v))
-- d unit = const 0
-- d (f zipp g) = d f (liftA2 zipp) d g
