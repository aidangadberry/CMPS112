s :: Integer -> String
s 0 = ""
s x
	| x == 1 = "one "
	| x == 2 = "two "
	| x == 3 = "three "
	| x == 4 = "four "
	| x == 5 = "five "
	| x == 6 = "six "
	| x == 7 = "seven "
	| x == 8 = "eight "
	| x == 9 = "nine "
	| x == 10 = "ten "
	| x == 11 = "eleven "
	| x == 12 = "twelve "
	| x == 13 = "thirteen "
	| x == 14 = "fourteen "
	| x == 15 = "fifteen "
	| x == 16 = "sixteen "
	| x == 17 = "seventeen "
	| x == 18 = "eighteen "
	| x == 19 = "nineteen "
	| y == 2 = "twenty " ++ s (x `mod` 10)
	| y == 3 = "thirty " ++ s (x `mod` 10)
	| y == 4 = "forty " ++ s (x `mod` 10)
	| y == 5 = "fifty " ++ s (x `mod` 10)
	| y == 6 = "sixty " ++ s (x `mod` 10)
	| y == 7 = "seventy " ++ s (x `mod` 10)
	| y == 8 = "eighty " ++ s (x `mod` 10)
	| y == 9 = "ninety " ++ s (x `mod` 10)
	| x `div` 100 > 0 = s (x `div` 100) ++ "hundred " ++ s (x `mod` 100)
	where y = x `div` 10

z :: Integer -> String
z 0 = ""
z x
	| x `div` u > 0 = s (x `div` u) ++ "vigintillion " ++ z (x `mod` u)
	| x `div` t > 0 = s (x `div` t) ++ "novemdecillion " ++ z (x `mod` t)
	| x `div` v > 0 = s (x `div` v) ++ "octodecillion " ++ z (x `mod` v)
	| x `div` r > 0 = s (x `div` r) ++ "septendecillion " ++ z (x `mod` r)
	| x `div` q > 0 = s (x `div` q) ++ "sexdecillion " ++ z (x `mod` q)
	| x `div` p > 0 = s (x `div` p) ++ "quindecillion " ++ z (x `mod` p)
	| x `div` o > 0 = s (x `div` o) ++ "quattuordecillion " ++ z (x `mod` o)
	| x `div` n > 0 = s (x `div` n) ++ "tredecillion " ++ z (x `mod` n)
	| x `div` m > 0 = s (x `div` m) ++ "duodecillion " ++ z (x `mod` m)
	| x `div` l > 0 = s (x `div` l) ++ "undecillion " ++ z (x `mod` l)
	| x `div` k > 0 = s (x `div` k) ++ "decillion " ++ z (x `mod` k)
	| x `div` j > 0 = s (x `div` j) ++ "nonillion " ++ z (x `mod` j)
	| x `div` i > 0 = s (x `div` i) ++ "octillion " ++ z (x `mod` i)
	| x `div` h > 0 = s (x `div` h) ++ "septillion "  ++ z (x `mod` h)
	| x `div` g > 0 = s (x `div` g) ++ "sextillion " ++ z (x `mod` g)
	| x `div` f > 0 = s (x `div` f) ++ "quintillion " ++ z (x `mod` f)
	| x `div` e > 0 = s (x `div` e) ++ "quadrillion " ++ z (x `mod` e)
	| x `div` d > 0 = s (x `div` d) ++ "trillion " ++ z (x `mod` d)
	| x `div` c > 0 = s (x `div` c) ++ "billion " ++ z (x `mod` c)
	| x `div` b > 0 = s (x `div` b) ++ "million " ++ z (x `mod` b)
	| x `div` a > 0 = s (x `div` a) ++ "thousand " ++ z (x `mod` a)
	| x `div` a == 0 = s x
	where {a = 1000; u = 1000000000000000000000000000000000000000000000000000000000000000;
	       b = 1000000; t = 1000000000000000000000000000000000000000000000000000000000000;
	       c = 1000000000; v = 1000000000000000000000000000000000000000000000000000000000;
	       d = 1000000000000; r = 1000000000000000000000000000000000000000000000000000000;
	       e = 1000000000000000; q = 1000000000000000000000000000000000000000000000000000;
		   f = 1000000000000000000; p = 1000000000000000000000000000000000000000000000000;
		   g = 1000000000000000000000; o = 1000000000000000000000000000000000000000000000; 
		   h = 1000000000000000000000000; n = 1000000000000000000000000000000000000000000;
		   i = 1000000000000000000000000000; m = 1000000000000000000000000000000000000000;
		   j = 1000000000000000000000000000000; l = 1000000000000000000000000000000000000;
		   k = 1000000000000000000000000000000000}