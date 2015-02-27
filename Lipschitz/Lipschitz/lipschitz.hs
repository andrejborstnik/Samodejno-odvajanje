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


-- |@constL@ je konstantna fukncija.
constL :: Fractional a => a -> L a
constL x = L x 0 0 infinity

-- | @idL@ je identična funkcija.
idL :: Fractional a => a -> L a
idL x = L x 1 1 infinity 

-- |
sqr :: Num a => a -> a
sqr a = a * a

-- it is beneficial to use points with the same eps, because we lose the least precision
instance (Fractional a, Ord a) => Num (L a) where
	fromInteger = constL . fromInteger
	L a au al aeps + L b bu bl beps = L (a + b) (au + bu) (al + bl) (min aeps beps)
	
	-- !!!!
	L a au al aeps * L b bu bl beps = L (a * b) au1 al1 eps where
		eps = min aeps beps
		aud = max (au * bu * eps + au * b + bu * a) (max (al * bl * eps + al * b + bl * a) (max (al * bu * eps + al * b + bu * a) (au * bl * eps + au * b + bl * a)))
		ald = min (al * bl * eps + al * b + bl * a) (min (al * bu * eps + al * b + bu * a) (min (au * bl * eps + au * b + bl * a) (au * bu * eps + au * b + bu * a)))
		aul = min (- al * bl * eps + al * b + bl * a) (min (- al * bu * eps + al * b + bu * a) (min (- au * bl * eps + au * b + bl * a) (- au * bu * eps + au * b + bu * a)))
		all = max (- au * bu * eps + au * b + bu * a) (max (- al * bl * eps + al * b + bl * a) (max (- al * bu * eps + al * b + bu * a) (- au * bl * eps + au * b + bl * a)))
		au1
			| (au < infinity && bu < infinity) = max aul aud
			| otherwise = infinity
		al1 
			|(al > - infinity && bl > - infinity) = min all ald
			| otherwise = - infinity
    
	negate (L a au al eps) = L (-a) (-al) (-au) eps
	signum (L a au al eps) = L (signum a) 0 0 eps
	
	abs (L a au al eps) = L (abs a) c (-c) eps  where
		c = max (abs au) (abs al)
	
instance (Fractional a, Ord a) => Fractional (L a) where
	fromRational = constL . fromRational
	
	-- foreach x \in [0, eps]. al * x <= f (a + x) - f a <= au * x
	-- because 1 / f (a + x) - 1 / f a =  f a - f (a + x) / f a * f (a + x), we have
	--  - au * x / fmax / f a <= 1 / f (a + x) - 1 / f a <= - al * x / fmin / f a
    -- podobno za x \in [- eps, 0]
  
	recip (L a au al eps) = L (recip a) au1 al1 eps1 where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		eps1
			| (a > 0 && (a - eps < 0 || a - amin < 0)) = a
			| (a < 0 && (a + eps > 0 || a + amax < 0)) = -a
			| (amax - a > eps || a - amin > eps) = max (amax - a) (a - amin)
			| otherwise = eps
		al1 
			| (a == infinity || eps1 == infinity || a == - infinity || a == 0) = -infinity
			| otherwise = (max (- au) (- al)) / amax / a
		au1 
			| (a == infinity || eps1 == infinity || a == - infinity || a == 0) = infinity
			| otherwise = (min (- al) (- au)) / amin / a

		
mod1 :: (Num a, Ord a) => a -> a -> a
mod1 x y = if x <= y && x >= 0 then x else if x > 0 then mod1 (x - y) y else mod1 (x + y) y
		
instance (Floating a, Ord a) => Floating (L a) where
	pi = constL pi
	exp (L a au al eps) = L (exp a) au1 al1 eps1 where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		eps1
			| (amax - a > eps || a - amin > eps) = max (amax - a) (a - amin)
			| otherwise = eps
		au1
			| (eps1 == infinity || a == infinity || a == - infinity) = infinity
			| otherwise = (exp (a + eps1) - exp a) / eps1
		al1 
			| (a == infinity || eps1 == infinity || a == - infinity) = 0
			| otherwise = (exp a - exp (a - eps1)) / eps1
			
	log (L a au al eps) = L (log a) au1 al1 eps1 where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		eps1
			| (a - eps < 0 || a - amin < 0) = a
			| (amax - a > eps || a - amin > eps) = max (amax - a) (a - amin)
			| otherwise = eps
		al1 
			| (a < 0) = infinity/infinity
			| (eps1 == infinity || a == infinity || a == 0) = 0
			| otherwise = (log (a + eps1) - log a) / eps1
		au1 
			| (a < 0) = infinity/infinity
			| (eps1 == infinity || a == infinity || a == 0) = infinity
			| otherwise = (log a - log (a - eps1)) / eps1
		
	sqrt (L a au al eps) = L (sqrt a) au1 al1 eps1 where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		eps1
			| (a < 0) = infinity/infinity
			| (a - eps < 0 || a - amin < 0) = a
			| (amax - a > eps || a - amin > eps) = max (amax - a) (a - amin)
			| otherwise = eps
		al1
			| (a < 0) = infinity/infinity
			| (eps1 == infinity || a == infinity || a == 0) = 0
			| otherwise = (sqrt (a + eps1) - sqrt a) / eps1
		au1
			| (eps1 == infinity || a == infinity || a == 0) = infinity
			| otherwise = (sqrt a - sqrt (a - eps1)) / eps1
		
	-- TODO, se da še izboljšati tist -1, 1, treba je pohandlat eps = neskoncno, a = +- neskoncno
	sin (L a au al eps) = L a au1 al1 eps where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		
		eps1
			| (amax - a > eps || a - amin > eps) = max (amax - a) (a - amin)
			| otherwise = eps
			
		au1 = 
			if eps > pi/4 then 
			    1
			else if amin `mod1` (2 * pi) >= 0 && amax `mod1` (2 * pi) <= pi/2 then
				(sin a - sin amin) / eps
			else if amin `mod1` (2 * pi) >= pi/2 && amax `mod1` (2 * pi) <= pi then
				(sin a - sin amin) / eps
			else if amin `mod1` (2 * pi) >= pi && amax `mod1` (2 * pi) <= 3 * pi / 2 then
				(sin amax - sin a) / eps
			else if amin `mod1` (2 * pi) >= 3 * pi / 2 && amax `mod1` (2 * pi) <= 2 * pi then
				(sin amax - sin a) / eps
			else 1
		al1 = 
			if eps > pi/4 then 
			    -1
			else if amin `mod1` (2 * pi) >= 0 && amax `mod1` (2 * pi) <= pi/2 then
				(sin amax - sin a) / eps
			else if amin `mod1` (2 * pi) >= pi/2 && amax `mod1` (2 * pi) <= pi then
				(sin amax - sin a) / eps
			else if amin `mod1` (2 * pi) >= pi && amax `mod1` (2 * pi) <= 3 * pi / 2 then
				(sin a - sin amin) / eps
			else if amin `mod1` (2 * pi) >= 3 * pi / 2 && amax `mod1` (2 * pi) <= 2 * pi then
				(sin a - sin amin) / eps
			else -1
			
		
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

		
	cos x = sin (pi/2 - x)
	-- asin -- naraščujoča, treba pogledat na kerem intervalu naj bi jo omejila
	-- acos -- padajoča, treba pogledat na kerem intervalu naj bi jo omejila
	atan (L a al au eps) = L (atan a) au1 al1 eps where
		amin = min (a - au * eps) (a + al * eps)
		amax = max (a + au * eps) (a - al * eps)
		au1
			| (eps == infinity || a == infinity || a == - infinity) = 1
			| otherwise = (atan amax - atan a) / eps
		al1 
			| (eps == infinity || a == infinity || a == - infinity) = 0
			| otherwise = (atan a - atan amin) / eps
	sinh x = (exp x - exp (-x)) / 2
	cosh x = (exp x + exp (-x)) / 2
	tanh x = sinh x / cosh x
	asinh x = log (x + sqrt (sqr x + 1))
	acosh x = log (x + sqrt (x - 1) * sqrt (x + 1))
	atanh x = log (1 + x) / 2 - log (1 - x) / 2

instance Eq a => Eq (L a) where
	L a _ _ _  == L b _ _ _  = (a == b)

rread :: L a -> (a, a, a)
rread (L a au al eps) = (a, au, al)

rread1 :: L a -> (a, a, a, a)
rread1 (L a au al eps) = (a, au, al, eps)

-- |Izračuna približek za določeni integral podane funkcije s korakom h.
integral :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integral f a1 a2 h = if a2 <= a1 then 0 else a + integral f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (L (a1 + interval) 1 1 interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = (zgornja + spodnja) / 2

-- |Izračuna (približno) zgornjo mejo za določeni integral podane funkcije s korakom h.
integralZ :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralZ f a1 a2 h = if a2 <= a1 then 0 else a + integralZ f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (L (a1 + interval) 1 1 interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	a = zgornja

-- |Izračuna (približno) spodnjo mejo za določeni integral podane funkcije s korakom h.
integralS :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralS f a1 a2 h = if a2 <= a1 then 0 else a + integralS f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (L (a1 + interval) 1 1 interval))
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = spodnja
	
	

f0 :: Floating a => a -> a
f0 z = z

f1 :: Floating a => a -> a
f1 z = 1 / (1 / z)

f2 :: Floating a => a -> a
f2 z = 5 * z * z

f3 :: Floating a => a -> a
f3 z = sqrt (2 * (1 / (1 / z)))

f4 :: Floating a => a -> a
f4 z = sqrt (2 * z)

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