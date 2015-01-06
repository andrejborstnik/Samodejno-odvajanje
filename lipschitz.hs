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

data L a = L a a a a a a -- value, upper lipschitz(on the right of the point), lower lipschitz, max value, min value, eps 

instance (Show a) => Show (L a) where
	show (L a b c d e f) = "L " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show f

infix 0 ><
(><) :: (Num a, Ord a) => (a -> a) -> (a -> a) -> (L a -> L a)
(f >< f') (L a au al amax amin eps) = L (f a) (au + c) (al + c) (max d (max e g)) (min e (min g d)) eps where
	c = f' a
	d = f amax
	e = f amin
	g = f a

infinity :: Fractional a => a
infinity = 1/0

con :: Fractional a => a
con = 0.01

eps :: Fractional a => a
eps = 0.01

-- how to simply create infinity? Replace Fractional back with Num when figured out.
constL :: Fractional a => a -> a -> a -> L a
constL x con eps = L x con (-con) (x + con*eps) (x - con*eps) eps  

sqr :: Num a => a -> a
sqr a = a * a

-- it is beneficial to use points with the same eps, because we lose the least precision
instance (Fractional a, Ord a) => Num (L a) where
	fromInteger = (\x -> constL x con eps) . fromInteger
	L a au al amax amin aeps + L b bu bl bmax bmin beps = L (a + b) (au + bu) (al + bl) (amax + bmax) (amin + bmin) (min aeps beps)
	L a au al amax amin aeps * L b bu bl bmax bmin beps = 
		L (a * b) (max (au * bu * eps + au * b + bu * a) (al * bl * eps + al * b + bl * a)) (min (al * bl * eps + al * b + bl * a) (min (al * bu * eps + al * b + bu * a) (au * bl * eps + au * b + bl * a))) (max (amax * bmax) (amin * bmin)) (min (amin * bmin) (min (amin * bmax) (amax * bmin))) eps where
		eps = min aeps beps
	negate (L a au al amax amin eps) = L (-a) (-al) (-au) (-amin) (-amax) eps
	signum = signum >< 0
	-- can abs be done better? Propably not.
	abs (L a au al amax amin aeps) = L (abs a) c (-c) d (-d) aeps  where
		c = max (abs au) (abs al)
		d = max (abs amax) (abs amin)
	
instance (Fractional a, Ord a) => Fractional (L a) where
	fromRational = (\x -> constL x con eps) . fromRational
	recip = recip >< - sqr recip

instance (Floating a, Ord a) => Floating (L a) where
	pi = constL pi con eps
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

instance Eq a => Eq (L a) where
	L a _ _ _ _ _ == L b _ _ _ _ _ = (a == b)

rread :: L a -> (a, a, a)
rread (L a au al amax amin eps) = (a, au, al)
	
integral :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integral f a1 a2 h = if a2 <= a1 then 0 else a + integral f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = abs((zgornja + spodnja) / 2)
	
integralz :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integralz f a1 a2 h = if a2 <= a1 then 0 else a + integralz f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval interval))
	zgornja = 2 * interval * x + interval * y / 2 - interval * z / 2
	a = abs(zgornja)

integrals :: (Floating a, Ord a) => (L a -> L a) -> a -> a -> a -> a
integrals f a1 a2 h = if a2 <= a1 then 0 else a + integrals f (a1 + h) a2 h where
	interval = if (a2 - a1 < h) then (a2 - a1) / 2 else h / 2
	(x, y, z) = rread (f (constL (a1 + interval) interval interval))
	spodnja = 2 * interval * x - interval * y / 2 + interval * z / 2
	a = abs(spodnja)
	
f1 :: Floating a => a -> a
f1 z = z + z + z

f2 :: Floating a => a -> a
f2 z = 3 * z

f3 :: Floating a => a -> a
f3 z = abs z

f4 :: Floating a => a -> a
f4 z = sqrt (sin z)

f5 :: Floating a => a -> a
f5 z = 1 - sqr (cos z)

f6 :: Floating a => a -> a
f6 z = sin z

f7 :: Floating a => a -> a
f7 z = cos z

f8 :: Floating a => a -> a
f8 z = 1 - sqr (cos z)

f9 z = z*z