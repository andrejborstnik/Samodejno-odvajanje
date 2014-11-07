import Control.Applicative


-- instance Functor ((->) t) where
	-- fmap f g = f . g

-- instance Applicative ((->) t) where
	-- pure = const
	-- f <*> g = \t -> (f t) (g t)

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
	
data D a = D a (D a) deriving (Show)

infix 0 ><
(><) :: Num a => (a -> a) -> (D a -> D a) -> (D a -> D a)
(f >< f') a@(D a0 a') = D (f a0) (a' * f' a)

sqr :: Num a => a -> a
sqr x = x * x

constD :: Num a => a -> D a
constD x = D x 0

idD :: Num a => a -> D a
idD x = D x 1

instance Num a => Num (D a) where
	fromInteger = constD . fromInteger
	D a a' + D b b' = D (a + b) (a' + b')
	x@(D x0 x') * y@(D y0 y') = D (x0 * y0) (x' * y + x * y')
	negate = negate >< -1
	signum = signum >< 0
	abs = abs >< signum
	
instance Fractional x => Fractional(D x) where
	fromRational = constD . fromRational
	recip = recip >< -sqr recip

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
	
	
	
	
	
	