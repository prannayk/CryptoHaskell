import Data.List
import Data.Hex
--xor operation
xor :: String->String->String
xor str1 str2 = zipWith (\a b -> if (a == b) then '0' else '1') str1 str2
--end
split :: String->Int->[String]
split _ 0 = []
split (a:b:c:d:e:f:xs) n = (a:b:c:d:e:f:[]):split xs (n-1)

process :: [Char] -> [Char]
process (x:xs) = let some = if (x == '1') then '0' else '1' in ([some] ++ [x] ++ xs ++ [some])
--end
--Converting to Binary from Hexadecimal and vice versa
hexatobin  :: String -> String
hexatobin [] = []
hexatobin (x:xs) = convert_binary x ++ hexatobin xs
convert_binary :: Char -> String
convert_binary a 
 | a == '0'= "0000"
 | a == '1' = "0001"
 | a =='2'= "0010"
 | a == '3'="0011"
 | a =='4'= "0100"
 | a =='5'= "0101"
 | a == '6'= "0110"
 | a =='7'= "0111"
 | a =='8'= "1000"
 | a =='9'= "1001"
 | a == 'A'= "1010" 
 | a =='B'= "1011"
 | a == 'C' = "1100"
 | a == 'D' = "1101"
 | a == 'E'= "1110"
 | a == 'F' = "1111"
 | otherwise = []

binarytohexa :: String->String
binarytohexa [] = []
binarytohexa (a:b:c:d:xs) = converthexa ([a]++[b] ++[c] ++[d]) ++ binarytohexa xs
converthexa :: String -> String 
converthexa a 
 | a == "0000" = "0"
 | a == "0001" = "1" 
 | a == "0010" = "2"
 | a == "0011" = "3"
 | a == "0100" = "4"
 | a == "0101" = "5"
 | a == "0110" = "6"
 | a == "0111" = "7"
 | a == "1000" = "8"
 | a == "1001" = "9"
 | a == "1010" = "A"
 | a == "1011" = "B"
 | a == "1100" = "C"
 | a == "1101" = "D"
 | a == "1110" = "E"
 | a == "1111" = "F"
 | otherwise = []
 --end
 --to convert the number to binary of number of characters n.
conv_to_bin :: Int -> Int ->[Char]
conv_to_bin _ 0 = []
conv_to_bin p n = (conv_to_bin (quot (p-a) 2) (n-1))++show a
		where a = p `mod` 2

binary_to_decimal :: String-> Int
binary_to_decimal instr 
 | instr == "00" = 0
 | instr == "01" = 1
 | instr == "10" = 2
 | instr == "11" = 3
 | instr == "0000" = 0
 | instr == "0001" = 1
 | instr == "0010" = 2
 | instr == "0011" = 3
 | instr == "0100" = 4
 | instr == "0101" = 5
 | instr == "0110" = 6
 | instr == "0111" = 7 
 | instr == "1000" = 8
 | instr == "1001" = 9 
 | instr == "1010" = 10
 | instr == "1011" = 11
 | instr == "1100" = 12
 | instr == "1101" = 13
 | instr == "1110" = 14
 | instr == "1111" = 15
 | otherwise = 56
 --end
--To generate 16 keys required of 48-bit from 64-bit
key ks n = gen (divKey ks n) pc2
 where pc2 = [14,17,11,24,1,5,3,28,15,6,21,10,23,19,12,4,26,8,16,7,27,20,13,2,41,52,31,37,47,55,30,40,51,45,33,48,44,49,39,56,34,53,46,42,50,36,29,32]
divKey :: String->Int->String
divKey ks n = let 
				(a,b) = splitAt (rot n m) (take 28 $ gen (hexatobin ks) pc1) 
				(x,y) = splitAt (rot n m) (drop 28 $ gen (hexatobin ks) pc1)
		   in b++a++y++x
 where 
 	pc1 = [57,49,41,33,25,17,9,1,58,50,42,34,26,18,10,2,59,51,43,35,27,19,11,3,60,52,44,36,63,55,47,39,31,23,15,7,62,54,46,38,30,22,14,6,61,53,45,37,29,21,13,5,28,20,12,4]
 	m = [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]
rot :: Int->[Int]->Int
rot n m = sum $ take n $ m

gen :: String->[Int]->String
gen _ [] = []
gen ls (p:pc1) = (ls!!(p-1)): gen ls pc1
--end
--function for using s-box (input in Decimal) 
enot :: String->String->[String]
enot rn_1 kn = split (xor (gen rn_1 e_bit) kn) 8
 where e_bit = [32,1,2,3,4,5,4,5,6,7,8,9,8,9,10,11,12,13,12,13,14,15,16,17,16,17,18,19,20,21,20,21,22,23,24,25,24,25,26,27,28,29,28,29,30,31,32,1]

conv::String->Int->String
conv xs n = conv_to_bin (index_to_index n (((binary_to_decimal (head xs : last xs : []))*16) + binary_to_decimal (tail $ init xs))) 4

index_to_index ::Int->Int->Int
index_to_index p n
 | p == 1 = [14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7,0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8,4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0,15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13] !! n
 | p == 2 = [15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10,3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5,0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15,13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9] !! n
 | p == 3 = [10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8,13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1,13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7,1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12] !! n
 | p == 4 = [7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15,13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9,10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4,3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14] !! n
 | p == 5 = [2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9,14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6,4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14,11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3] !! n
 | p == 6 = [12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11,10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8,9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6,4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13] !! n
 | p == 7 = [4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1,13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6,1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2,6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12] !! n
 | p == 8 = [13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7,1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2,7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8,2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11] !! n
 | otherwise = 56

--function that operates on rn-1 and kn
function :: String->String->Int->String
function rn ks n = let 
						a:b:c:d:e:f:g:h:[]=enot rn (key ks n)
						p=[16,7,20,21,29,12,28,17,1,15,23,26,5,18,31,10,2,8,24,14,32,27,3,9,19,13,30,6,22,11,4,25]
					in 
						gen ((conv a 1)++(conv b 2)++(conv c 3)++(conv d 4)++(conv e 5)++(conv f 6)++(conv g 7)++(conv h 8)) p
--end 
--main function for encryption				   
des :: String->String->String
des abce ky = binarytohexa (gen (encrypt xs sx ky 16) ip2)
 where 
       xs = take 32 $ gen (hexatobin abce) ip 
       sx = drop 32 $ gen (hexatobin abce) ip 
       ip = [58,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,62,54,46,38,30,22,14,6,64,56,48,40,32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7]
       ip2= [40,8,48,16,56,24,64,32,39,7,47,15,55,23,63,31,38,6,46,14,54,22,62,30,37,5,45,13,53,21,61,29,36,4,44,12,52,20,60,28,35,3,43,11,51,19,59,27,34,2,42,10,50,18,58,26,33,1,41,9,49,17,57,25]
encrypt :: String->String->String->Int->String
encrypt xs sx k 0 = sx++xs
encrypt xs sx k n = let ls = (xor xs (function sx k (17-n))) in encrypt sx ls k (n-1)
--end
--Decryption of DES
indes :: String->String->String
indes abce ky = binarytohexa (gen (decrypt sx xs ky 16) ip2)
 where 
       xs = take 32 $ gen (hexatobin abce) ip
       sx = drop 32 $ gen (hexatobin abce) ip 
       ip = [58,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,62,54,46,38,30,22,14,6,64,56,48,40,32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7]
       ip2= [40,8,48,16,56,24,64,32,39,7,47,15,55,23,63,31,38,6,46,14,54,22,62,30,37,5,45,13,53,21,61,29,36,4,44,12,52,20,60,28,35,3,43,11,51,19,59,27,34,2,42,10,50,18,58,26,33,1,41,9,49,17,57,25]
decrypt :: String->String->String->Int->String
decrypt xs sx k 0 = xs++sx
decrypt xs sx k n = let ls = (xor sx (function xs k n)) in decrypt ls xs k (n-1)
--end

