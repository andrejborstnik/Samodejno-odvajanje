import Control.Applicative


-- instance Functor ((->) t) where
	-- fmap f g = f . g

-- instance Applicative ((->) t) where
	-- pure = const
	-- f <*> g = \t -> (f t) (g t)

data D a = D a a deriving (Show)

constD :: Num a => a -> D a
constD x = D x 0

idD :: Num a => a -> D a
idD x = D x 1

instance Num a => Num (D a) where
	fromInteger x = constD (fromInteger x)
	(+) (D a a') (D b b') = D (a + b) (a' + b')
	(-) (D a a') (D b b') = D (a - b) (a' - b')
	(*) (D a a') (D b b') = D (a * b) (b' * a + a' * b)
	negate (D a a') = D (negate a) (negate a')
	signum (D a _) = D (signum a) 0
	abs (D a a') = D (abs a) (a' * (signum a))
	
instance Fractional x => Fractional(D x) where
	fromRational x = constD (fromRational x)
	-- (/) (D a a') (D b b') = D (a/b) ((b' * a + a' * b)/(b^2))
	recip (D x x') = D (recip x) (-x' / sqr x)

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

infix 0 ><
(><) :: Num a => (a -> a) -> (a -> a) -> (D a -> D a)
(f >< f') (D a a') = D (f a) (a' * f' a)

sqr :: Num a => a -> a
sqr x = x * x


instance Floating x => Floating (D x) where
	pi = D pi 0
	exp = exp >< exp
	log = log >< recip
	sqrt = sqrt >< \x -> recip (2 * sqrt x)
	sin = sin >< cos
	cos = cos >< \x -> - sin x
	asin = asin >< \x -> recip (sqrt (1 - sqr x))
	acos = acos >< \x -> - recip (sqrt (1 - sqr x))

	
	
	
	
	
	