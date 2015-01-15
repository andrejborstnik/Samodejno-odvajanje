{-|
Module      : Lipschitz
Description :  Omejitev funkcij z premicami (koeficientom pravimo Lipschitzove konstante)
Copyright   : (c) Andrej Borštnik, Barbara Bajcer, 2015
Maintainer  : barbara.bajcer@gmail.com, andrej-borstnik@hotmail.com
Stability   : experimental 

Računanje vrednosti funkcije in konstant, ki lokalno omejujejo to funkcijo.
-}

module Lipschitz.Lipschitz
( L(..)
, constL
, idL
, sqr
, integralR
, integralRz
, integralRs
, integralF
, integralFz
, integralFs
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

-- |Struktura L predstavlja točko, zgornjo konstanto (desno od točke), spodnjo konstanto ter okolico, kjer premici omejujeta funkcijo.
data L a = L a a a a

instance (Show a) => Show (L a) where
	show (L a b c d) = "L " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d

-- infix 0 ><
-- (><) :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> (L a -> L a)
-- (f >< f') (L a au al eps) = L v (au + c) (al + c) eps where
	-- v = f a
	-- c = f' a
	-- leftvalue = f (a - eps)
	-- leftup = (al-1 + c)*(-eps) + v
	-- leftdown = (au -1+ c)*(-eps) + v
	-- rightvalue = f (a + eps)
	-- rightup = (au-1 + c)*(eps) + v
	-- rightdown = (al -1+ c)*(eps) + v
	-- k1 = max ((rightvalue - v)/eps) (max ((v - leftdown)/eps) ((v - leftvalue)/eps))
	-- k2 = min ((rightvalue - v)/eps) (min ((rightdown - v)/eps) ((v - leftvalue)/eps))

infinity :: Fractional a => a
infinity = 1/0

eps :: Fractional a => a
eps = 0.01

-- |@constL@ je konstantna fukncija.
constL :: Fractional a => a -> a -> a -> L a
constL x eps = L x 0 0 eps

-- | @idL@ je identična funkcija.
idL :: Fractional a => a -> a -> a -> L a
idL x eps = L x 1 1 eps    

-- |
sqr :: Num a => a -> a
sqr a = a * a

-- it is beneficial to use points with the same eps, because we lose the least precision
instance (Fractional a, Ord a) => Num (L a) where
	fromInteger = (\x -> constL x eps) . fromInteger
	L a au al aeps + L b bu bl beps = L (a + b) (au + bu) (al + bl) (min aeps beps)
	
	-- !!!!
	L a au al aeps * L b bu bl beps = 
		L (a * b) (max (au * bu * eps + au * b + bu * a) (al * bl * eps + al * b + bl * a)) (min (al * bl * eps + al * b + bl * a) (min (al * bu * eps + al * b + bu * a) (au * bl * eps + au * b + bl * a))) eps where
		eps = min aeps beps
		
	negate (L a au al eps) = L (-a) (-al) (-au) eps
	signum = signum >< 0
	
	-- abs (L a au al aeps) = L (abs a) ((signum a)*au) ((signum a)*al) aeps
	-- can abs be done better? Propably not.
	abs (L a au al aeps) = L (abs a) c (-c) aeps  where
		c = max (abs au) (abs al)
	
instance (Fractional a, Ord a) => Fractional (L a) where
	fromRational = (\x -> constL x eps) . fromRational
	-- TODO finish him
	recip (L a au al eps) = L (recip a) au al eps where
		al1 = (log (a + eps) - a) / eps
		au1 = (a - log (1 - eps)) / eps

instance (Floating a, Ord a) => Floating (L a) where
	pi = constL pi eps
	exp (L a au al eps) = L a au1 al1 eps where
		au1 = (exp (a + eps) - a) / eps
		al1 = (a - exp (1 - eps)) / eps
	log (L a au al eps) = L a au1 al1 eps where
		al1 = (log (a + eps) - a) / eps
		au1 = (a - log (1 - eps)) / eps
	sqrt (L a au al eps) = L a au1 al1 eps where
		al1 = (sqrt (a + eps) - a) / eps
		au1 = (a - sqrt (1 - eps)) / eps
	-- TODO
	sin (L a au al eps) = L a au al eps where
		amin = a - eps
		amax = a + eps
		-- if eps >= pi/2 then
			-- if a % 2 * pi \in [-pi/2, pi/2] then 
				-- au1 = cos a -- to se tudi da izboljšat
				-- al1 = - cos a -- to je samo spodnja meja, lahko se jo še izboljšat, sploh če epx < pi
			-- else
				-- al1 = cos a
				-- au1 = - cos a
		-- if interval [amin % 2 * pi, amax % 2 * pi] \subset [0, pi/4] then 
			-- al1 = (sin (a + eps) - a) / eps
			-- au1 = (a - sin (1 - eps)) / eps
		-- else if .. \subset [pi/2, pi]
			-- al1 = (sin (a + eps) - a) / eps
			-- au1 = (a - sin (1 - eps)) / eps
		-- else if .. \subset [pi, 3 * pi/2]
			-- au1 = (sin (a + eps) - a) / eps
			-- al1 = (a - sin (1 - eps)) / eps
		-- else if .. \subset [3 * pi/2, 2*pi]
			-- au1 = (sin (a + eps) - a) / eps
			-- al1 = (a - sin (1 - eps)) / eps
		-- ce gre pa cez vec intervalov ...

		
	-- cos = cos >< - sin -- podobno kot sin
	-- asin = asin >< recip (sqrt (1 - sqr))
	-- acos = acos >< - recip (sqrt (1 - sqr))
	-- atan = atan >< recip (1 + sqr)
	-- sinh = sinh >< cosh
	-- cosh = cosh >< sinh
	-- asinh = asinh >< recip (sqrt (1 + sqr))
	-- acosh = acosh >< recip (sqrt (- 1 + sqr))
	-- atanh = asin >< recip (1 - sqr)

instance Eq a => Eq (L a) where
	L a _ _ _  == L b _ _ _  = (a == b)

rread :: L a -> (a, a, a)
rread (L a au al eps) = (a, au, al)

rread1 :: L a -> (a, a, a, a)
rread1 (L a au al eps) = (a, au, al, eps)

-- |Izračuna približek za določeni integral podane racionalne funkcije s korakom h.
integralR :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralR f a1 a2 h = if a2 <= a1 then 0 else a + integralR f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (idL (a1 + interval) interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = (zgornja + spodnja) / 2

-- |Izračuna (približno) zgornjo mejo za določeni integral podane racionalne funkcije s korakom h.
integralRz :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralRz f a1 a2 h = if a2 <= a1 then 0 else a + integralRz f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (idL (a1 + interval) interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	a = zgornja

-- |Izračuna (približno) spodnjo mejo za določeni integral podane racionalne funkcije s korakom h.
integralRs :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralRs f a1 a2 h = if a2 <= a1 then 0 else a + integralRs f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (idL (a1 + interval) interval))
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = spodnja

-- |Izračuna približek za določeni integral podane Floating funkcije s korakom h.
integralF :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralF f a1 a2 h = if a2 <= a1 then 0 else a + integralF f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = (zgornja + spodnja) / 2

-- |Izračuna (približno) zgornjo mejo za določeni integral podane Floating funkcije s korakom h.
integralFz :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralFz f a1 a2 h = if a2 <= a1 then 0 else a + integralFz f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	a = zgornja

-- |Izračuna (približno) spodnjo mejo za določeni integral podane Floating funkcije s korakom h.
integralFs :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralFs f a1 a2 h = if a2 <= a1 then 0 else a + integralFs f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval))
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = spodnja
	
	
	
	

f0 :: Floating a => a -> a
f0 z = z

f1 :: Floating a => a -> a
f1 z = z+z+z + z + z

f2 :: Floating a => a -> a
f2 z = 5 * z

f3 :: Floating a => a -> a
f3 z = sin (5 * z)

f4 :: Floating a => a -> a
f4 z = cos(sqrt (sin z))

f5 :: Floating a => a -> a
f5 z = 1 - sqr (cos z)


f7 :: Floating a => a -> a
f7 z = (cos z) * z

f8 :: Floating a => a -> a
f8 z = 1 - sqr (cos z)

f9 z = z*z*z*z - 5*z*z*z + 2*z*z - 1

f10 :: Floating a => a -> a
f10 z = exp z * sin z

f11 :: Floating a => a -> a
f11 z = sqr (sqr z) + 6 * (sqr z) * z + 2 * (sqr z) + 71