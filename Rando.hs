import System.Random
main = do 
	gen <- getStdGen
	let 
		xs = randomRs (100000000000000000000000000000, 10000000000000000000000000000000000) (gen) :: [Integer]
		a1 = find_a_prime xs
		sx = filter (/=a1) xs
		a2 = find_a_prime sx
		s = a1*a2
	print a1
	print a2
	print s
find_a_prime :: [Integer]->Integer
find_a_prime (x:xs)=if (primality_test x 1000)
						then x
						else find_a_prime xs

primality_test :: Integer -> Integer -> Bool
primality_test p step = if (carmicheal p)
							then False
							else run_test p step (x:xs)
							
	where (x:xs)=randomRs (2,p-1) (mkStdGen.fromInteger $ p`mod`step) :: [Integer]

--for removing Carmicheal numbers.
carmicheal :: Integer -> Bool
carmicheal p = if ((p-1)`mod`6==0 || (p+1)`mod`6 ==0)
				then False
				else True

-- for checking of the prime using Baye's theorem
run_test :: Integer -> Integer -> [Integer] -> Bool
run_test _ 0 _ = True
run_test p step (x:xs) = if ((find_gcd x p) == 1)
							then ( if ((find_mod p x) == 1) 
									then run_test p (step-1) xs
									else False
								 )
							else run_test p step xs
-- for checking co-primes
find_gcd :: Integer -> Integer -> Integer
find_gcd x 0 = x
find_gcd 0 p = p
find_gcd x p = if (x > p)
				then find_gcd (x`mod`p) p
				else find_gcd x (p`mod`x)

--for converting type of function take
take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' p (x:xs) = x : take' (p - 1) xs

--for checking the fermat's little theorem
find_mod :: Integer -> Integer -> Integer
find_mod p x = foldl1 (*) (filter (/=0) (zipWith (*) xs (x:(ls p x (length xs)))))`mod` p 
			where xs = conv_to_bin (p-1)
ls :: Integer -> Integer -> Int -> [Integer]
ls _ _ 0 = []
ls p x len = m : (ls p m (len-1))
		where m = (x*x)`mod`p 
--Converting to binary digit
conv_to_bin :: Integer -> [Integer]
conv_to_bin 0 = []
conv_to_bin p = a : conv_to_bin (quot (p-a) 2)
		where a = p `mod` 2
	