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
, integral
, integralZ
, integralS
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

-- |Struktura L predstavlja točko, zgornjo konstanto (desno od točke), spodnjo konstanto ter okolico, kjer premici omejujeta funkcijo (levi eps in desni eps).
data L a = L a a a a a

instance (Show a) => Show (L a) where
	show (L a b c d e) = "L " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e

infinity :: Fractional a => a
infinity = 1/0

-- |@constL@ je konstantna fukncija.
constL :: Fractional a => a -> L a
constL x = L x 0 0 infinity infinity

-- | @idL@ je identična funkcija.
idL :: Fractional a => a -> L a
idL x = L x 1 1 infinity infinity

-- |
sqr :: Num a => a -> a
sqr a = a * a

-- it is beneficial to use points with the same eps, because we lose the least precision
instance (Fractional a, Ord a) => Num (L a) where
	fromInteger = constL . fromInteger
	L a au al aeps1 aeps2 + L b bu bl beps1 beps2 = L (a + b) (au + bu) (al + bl) eps1 eps2 where
		eps1
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = beps1
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = aeps1
			| otherwise = (aeps1 + beps1)/2
		eps2 
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = beps2
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = aeps2
			| otherwise = (aeps2 + beps2)/2
	
	L a au al aeps1 aeps2 * L b bu bl beps1 beps2 = L (a * b) u l eps1 eps2 where
		eps1
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = min aeps1 beps1
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = min aeps1 beps1
			| otherwise = (aeps1 + beps1)/2
		eps2 
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = min aeps2 beps2
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = min aeps2 beps2
			| otherwise = (aeps2 + beps2)/2
		u1 
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = a*bu
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = b*au
			| otherwise =  max (au * bu * eps2 + au * b + bu * a) (max (al * bl * eps2 + al * b + bl * a) (max (- al * bu * eps1 + al * b + bu * a) (- au * bl * eps1 + au * b + bl * a)))
		l1
			| (aeps1 == infinity && aeps2 == infinity && au == 0 && al == 0) = bl*a
			| (beps1 == infinity && beps2 == infinity && bu == 0 && bl == 0) = al*b
			| otherwise = min (au * bl * eps2 + au * b + bl * a) (min (al * bu * eps2 + al * b + bu * a) (min (- al * bl * eps1 + al * b + bl * a) (- au * bu * eps1 + au * b + bu * a)))
		u
			| (au < infinity && bu < infinity) = u1
			| otherwise = infinity
		l 
			|(al > - infinity && bl > - infinity) = l1
			| otherwise = - infinity
    
	negate (L a au al eps1 eps2) = L (-a) (-al) (-au) eps1 eps2
	signum (L a au al eps1 eps2) = L (signum a) 0 0 eps1 eps2
	
	abs (L a au al eps1 eps2) = L (abs a) c (-c) eps1 eps2  where
		c = max (abs au) (abs al)


instance (Fractional a, Ord a) => Fractional (L a) where
	fromRational = constL . fromRational
	
	-- Razlaga: 
	-- foreach x \in [0, eps]. al * x <= f (a + x) - f a <= au * x
	-- because 1 / f (a + x) - 1 / f a =  f a - f (a + x) / f a * f (a + x), we have
	--  - au * x / fmax / f a <= 1 / f (a + x) - 1 / f a <= - al * x / fmin / f a
    -- podobno za x \in [- eps, 0]
  
	recip (L a au al e1 e2) = L (recip a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 
			| (a > 0) = min a (a - amin)
			| otherwise = a - amin
		eps2
			| (a < 0) = min (- a) (amax - a)
			| otherwise = amax - a
		l 
			| (a == infinity || eps1 == infinity || eps2 == infinity || a == - infinity || a == 0) = -infinity
			| otherwise = (max (- au) (- al)) / amax / a
		u 
			| (a == infinity || eps1 == infinity || eps2 == infinity || a == - infinity || a == 0) = infinity
			| otherwise = (min (- al) (- au)) / amin / a

		
mod1 :: (Num a, Ord a) => a -> a -> a
mod1 x y = if x <= y && x >= 0 then x else if x > 0 then mod1 (x - y) y else mod1 (x + y) y
		
		
instance (Floating a, Ord a) => Floating (L a) where
	pi = constL pi
	exp (L a au al e1 e2) = L (exp a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 = a - amin
		eps2 = amax - a
		l 
			| (a == infinity || eps1 == infinity || eps2 == infinity || a == - infinity) = -infinity
			| otherwise = (exp a - exp (a - eps1)) / eps1
		u 
			| (a == infinity || eps1 == infinity || eps2 == infinity || a == - infinity) = infinity
			| otherwise = (exp (a + eps2) - exp a) / eps2
			
	log (L a au al e1 e2) = L (log a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 
			| (a > 0) = min a (a - amin)
			| otherwise = a - amin
		eps2
			| (a < 0) = min (- a) (amax - a)
			| otherwise = amax - a
		l 
			| (a <= 0) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity || a == infinity) = 0
			| otherwise = (log (a + eps2) - log a) / eps2
		u 
			| (a <= 0) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity || a == infinity) = infinity
			| otherwise = (log a - log (a - eps1)) / eps1
		
	sqrt (L a au al e1 e2) = L (sqrt a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 
			| (a > 0) = min a (a - amin)
			| otherwise = a - amin
		eps2
			| (a < 0) = min (- a) (amax - a)
			| otherwise = amax - a
		l
			| (a < 0) = infinity/infinity
			| (eps1 == infinity || a == infinity || a == 0) = 0
			| otherwise = (sqrt (a + eps2) - sqrt a) / eps2
		u
			| (a < 0) = infinity/infinity
			| (eps1 == infinity || a == infinity || a == 0) = infinity
			| otherwise = (sqrt a - sqrt (a - eps1)) / eps1
		
	sin (L a au al e1 e2) = L (sin a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 = a - amin
		eps2 = amax - a			
		u = 1
			-- if eps > pi/4 then 
			    -- 1
			-- else if amin `mod1` (2 * pi) >= 0 && amax `mod1` (2 * pi) <= pi/2 then
				-- (sin a - sin amin) / eps
			-- else if amin `mod1` (2 * pi) >= pi/2 && amax `mod1` (2 * pi) <= pi then
				-- (sin a - sin amin) / eps
			-- else if amin `mod1` (2 * pi) >= pi && amax `mod1` (2 * pi) <= 3 * pi / 2 then
				-- (sin amax - sin a) / eps
			-- else if amin `mod1` (2 * pi) >= 3 * pi / 2 && amax `mod1` (2 * pi) <= 2 * pi then
				-- (sin amax - sin a) / eps
			-- else 1
		l = -1
			-- if eps > pi/4 then 
			    -- -1
			-- else if amin `mod1` (2 * pi) >= 0 && amax `mod1` (2 * pi) <= pi/2 then
				-- (sin amax - sin a) / eps
			-- else if amin `mod1` (2 * pi) >= pi/2 && amax `mod1` (2 * pi) <= pi then
				-- (sin amax - sin a) / eps
			-- else if amin `mod1` (2 * pi) >= pi && amax `mod1` (2 * pi) <= 3 * pi / 2 then
				-- (sin a - sin amin) / eps
			-- else if amin `mod1` (2 * pi) >= 3 * pi / 2 && amax `mod1` (2 * pi) <= 2 * pi then
				-- (sin a - sin amin) / eps
			-- else -1
			
		
		-- if eps >= pi/4 then
			-- au1 = 1 -- to se tudi da izboljšat
			-- al1 = - 1 -- to je samo spodnja meja, lahko se jo še izboljšat, sploh če epx < pi
		-- else 
			-- if amin `mod` (2 * pi) >= 0 && amax `mod` (2 * pi) <= pi/2 then 
				-- al1 = (sin amax - a) / eps
				-- au1 = (a - sin amin) / eps
			-- else if amin `mod` (2 * pi) >= pi/2 && amax `mod` (2 * pi) <= pi then
				-- al1 = (sin amax - a) / eps
				-- au1 = (a - sin amin) / eps
			-- else if amin `mod` (2 * pi) >= pi && amax `mod` (2 * pi) <= 3 * pi / 2
				-- au1 = (sin amax - a) / eps
				-- al1 = (a - sin amin) / eps
			-- else if amin `mod` (2 * pi) >= 3 * pi / 2 && amax `mod` (2 * pi) <= 2 * pi
				-- au1 = (sin amax - a) / eps
				-- al1 = (a - sin amin) / eps
		-- kaj pa če je čez več intervalov?

		
	cos (L a au al e1 e2) = L (cos a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 = a - amin
		eps2 = amax - a			
		u = 1
		l = -1
	
	asin (L a au al e1 e2) = L (asin a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1 
			| (a > -1) = min (a + 1) (a - amin)
			| otherwise = a - amin
		eps2
			| (a < 1) = min (1 - a) (amax - a)
			| otherwise = amax - a
		l
			| (a <= -1) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity) = infinity
			| otherwise = infinity -- TEŽAVE, KER ODVOD NI MONOTON
		u
			| (a >= 1) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity) = infinity
			| otherwise = infinity -- TEŽAVE, KER ODVOD NI MONOTON
			
	acos (L a au al e1 e2) = L (acos a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1
			| (a > -1) = min (a + 1) (a - amin)
			| otherwise = a - amin
		eps2
			| (a < 1) = min (1 - a) (amax - a)
			| otherwise = amax - a
		l
			| (a <= -1) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity) = infinity
			| otherwise = infinity  -- TEŽAVE, KER ODVOD NI MONOTON
		u
			| (a >= 1) = infinity/infinity
			| (eps1 == infinity || eps2 == infinity) = infinity
			| otherwise = infinity  -- TEŽAVE, KER ODVOD NI MONOTON
			
	atan (L a al au e1 e2) = L (atan a) u l eps1 eps2 where
		amin = min (a - au * e1) (a + al * e2)
		amax = max (a + au * e2) (a - al * e1)
		eps1
			| (a > -pi) = min (a + pi) (a - amin)
			| otherwise = a - amin
		eps2
			| (a < pi) = min (pi - a) (amax - a)
			| otherwise = amax - a
		u
			| (eps1 == infinity || eps2 == infinity || a == infinity || a == - infinity) = 1
			| otherwise = 1 -- TEŽAVE, KER ODVOD NI MONOTON
		l 
			| (eps1 == infinity || eps2 == infinity || a == infinity || a == - infinity) = 0
			| otherwise = 0 -- TEŽAVE, KER ODVOD NI MONOTON
			
	sinh x = (exp x - exp (-x)) / 2
	cosh x = (exp x + exp (-x)) / 2
	tanh x = sinh x / cosh x
	asinh x = log (x + sqrt (sqr x + 1))
	acosh x = log (x + sqrt (x - 1) * sqrt (x + 1))
	atanh x = log (1 + x) / 2 - log (1 - x) / 2

instance Eq a => Eq (L a) where
	L a _ _ _ _ == L b _ _ _ _ = (a == b)

rread :: L a -> (a, a, a)
rread (L a au al eps1 eps2) = (a, au, al)

rread1 :: L a -> (a, a, a, a, a)
rread1 (L a au al eps1 eps2) = (a, au, al, eps1, eps2)

lip :: (Floating a, Ord a) =>(L a -> L a) -> a -> a -> a -> L a
lip f a e1 e2 = f (L a 1 1 e1 e2)

-- |Izračuna približek za določeni integral podane funkcije s korakom h.
integral :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integral f a1 a2 h = if a2 <= a1 then 0 else a + integral f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, u, l, e1, e2) = rread1 (f (L (a1 + interval) 1 1 interval interval)) 
	zgornja = x * (e1 + e2) - e1 * l * e1 / 2 + e2 * u * e2 / 2
	spodnja = x * (e1 + e2) - e1 * u * e1 / 2 + e2 * l * e2 / 2
	a = (zgornja + spodnja) / 2

-- |Izračuna (približno) zgornjo mejo za določeni integral podane funkcije s korakom h.
integralZ :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralZ f a1 a2 h = if a2 <= a1 then 0 else a + integralZ f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, u, l, e1, e2) = rread1 (f (L (a1 + interval) 1 1 interval interval))
	a = x * (e1 + e2) - e1 * l * e1 / 2 + e2 * u * e2 / 2

-- |Izračuna (približno) spodnjo mejo za določeni integral podane funkcije s korakom h.
integralS :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralS f a1 a2 h = if a2 <= a1 then 0 else a + integralS f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, u, l, e1, e2) = rread1 (f (L (a1 + interval) 1 1 interval interval))
	a = x * (e1 + e2) - e1 * u * e1 / 2 + e2 * l * e2 / 2

	
f0 :: Floating a => a -> a
f0 z = 3 * z

f1 :: Floating a => a -> a
f1 z = 1 / (1 / z)

f2 :: Floating a => a -> a
f2 z = 5 * z * z

f3 :: Floating a => a -> a
f3 z = sqrt (2 * (1 / (1 / z)))

f4 :: Floating a => a -> a
f4 z = sqrt (z)

f5 :: Floating a => a -> a
f5 z = sqr (cos z)

f7 :: Floating a => a -> a
f7 z = (cos z)

f8 :: Floating a => a -> a
f8 z = 1 - sqr (cos z)

f9 z = z*z*z*z - 5*z*z*z + 2*z*z - 1

f10 :: Floating a => a -> a
f10 z = exp z * sin z

f11 :: Floating a => a -> a
f11 z = sqr (sqr z) + 6 * (sqr z) * z + 2 * (sqr z) + 71