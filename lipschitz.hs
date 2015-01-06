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

data L a = L a a a a -- value, upper lipschitz(on the right of the point), lower lipschitz, eps 

instance (Show a) => Show (L a) where
	show (L a b c d) = "L " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d

infix 0 ><
(><) :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> (L a -> L a)
(f >< f') (L a au al eps) = L v k1 k2 eps where
	v = f a
	c = f' a
	leftvalue = f (a + eps)
	leftup = (al + c)*(-eps) + v
	leftdown = (au + c)*(-eps) + v
	rightvalue = f (a - eps)
	rightup = (au + c)*(eps) + v
	rightdown = (al + c)*(eps) + v
	k1 = max ((rightvalue - v)/eps) (max ((v - leftdown)/eps) ((v - leftvalue)/eps))
	k2 = min ((rightvalue - v)/eps) (min ((rightdown - v)/eps) ((v - leftvalue)/eps))

infinity :: Fractional a => a
infinity = 1/0

con :: Fractional a => a
con = 0.01

eps :: Fractional a => a
eps = 0.01

constL :: Fractional a => a -> L a
constL x = L x con (-con) eps  

sqr :: Num a => a -> a
sqr a = a * a

-- it is beneficial to use points with the same eps, because we lose the least precision
instance (Fractional a, Ord a) => Num (L a) where
	fromInteger = constL . fromInteger
	L a au al aeps + L b bu bl beps = L (a + b) (au + bu) (al + bl) (min aeps beps)
	L a au al aeps * L b bu bl beps = 
		L (a * b) (max (au * bu * eps + au * b + bu * a) (al * bl * eps + al * b + bl * a)) (min (al * bl * eps + al * b + bl * a) (min (al * bu * eps + al * b + bu * a) (au * bl * eps + au * b + bl * a))) eps where
		eps = min aeps beps
	negate (L a au al eps) = L (-a) (-al) (-au) eps
	signum = signum >< 0
	-- can abs be done better? Propably not.
	abs (L a au al aeps) = L (abs a) c (-c) aeps  where
		c = max (abs au) (abs al)
	
instance (Fractional a, Ord a) => Fractional (L a) where
	fromRational = constL . fromRational
	recip = recip >< - sqr recip

instance (Floating a, Ord a) => Floating (L a) where
	pi = constL pi
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
	L a _ _ _  == L b _ _ _  = (a == b)
	
-- instance Ord a => Ord (L a) where
	-- min x@(D a al' ar') y@(D b bl' br')
		-- | a < b || (al' > bl' && ar' < br') = x
		-- | b < a || (bl' > al' && ar' > br') = y
		-- | al' > bl' = D a al' br'
		-- | otherwise = D a bl' ar'
		
	-- max x@(D a al' ar') y@(D b bl' br')
		-- | a < b || (al' < bl' && ar' > br') = x
		-- | b < a || (bl' < al' && ar' < br') = y
		-- | al' < bl' = D a al' br'
		-- | otherwise = D a bl' ar'
			
	-- (<=) (D a _ _) (D b _ _) = (<=) a b
	
f1 :: Floating a => a -> a
f1 z = z + z + z

f2 :: Floating a => a -> a
f2 z = 3 * z

f3 :: Floating a => a -> a
f3 z = abs z

f4 :: Floating a => a -> a
f4 z = sqr (sin z)

f5 :: Floating a => a -> a
f5 z = 1 - sqr (cos z)

f6 :: Floating a => a -> a
f6 z = sin z

f7 :: Floating a => a -> a
f7 z = cos z

f8 :: Floating a => a -> a
f8 z = 1 - sqr (cos z)

f9 z = z*z