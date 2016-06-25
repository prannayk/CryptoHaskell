import Data.List
e :: String->String
e xs = map (\x-> if x == '1' then '0' else '1') (filt xs (length xs))
filt::String->Int->String
filt _ -3 = []
filt xs n = (xs !! n-1):filt xs n-4