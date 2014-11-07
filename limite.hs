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


data D a = D a a a deriving Show

infix 0 ><
(><) :: Num a => (a -> a) -> (a -> a) -> (D a -> D a)
(f >< f') (D a al' ar') = D (f a) (al' * f' a) (ar' * f' a)

constD :: Num a => a -> D a
constD x = D x 0 0

idD :: Num a => a -> D a
idD x = D x 1 1

sqr :: Num a => a -> a
sqr a = a * a

instance Num a => Num (D a) where
	fromInteger = constD . fromInteger
	D a al' ar' + D b bl' br' = D (a + b) (al' + bl') (ar' + br')
	(D x xl' xr') * (D y yl' yr') = D (x * y) (xl' * y + yl' * x) (xr' * y + yr' * x)
	negate = negate >< -1
	signum = signum >< 0
	abs = abs >< signum

instance Fractional a => Fractional (D a) where
	fromRational = constD . fromRational
	recip = recip >< -sqr recip

instance Floating a => Floating (D a) where
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
	(<=) (D a _ _) (D b _ _) = (<=) a b
	
f1 :: Floating a => a -> a
f1 z = sqrt ((sqr z) * 3 * sin z)

f2 :: (Ord a, Floating a) => a -> a
f2 z = min (sin z) 0
