split :: String->Int->[String]
split _ 0 = []
split (a:b:c:d:xs) n = (a:b:c:d:[]):split xs (n-1)

process :: [Char] -> [Char]
process (x:xs) = let some = if (x == '1') then '0' else '1' in ([some] ++ [x] ++ xs ++ [some])

enot :: String -> Int -> [Char]
enot list n = ls>>= process
 where ls = split list n 