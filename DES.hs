--  XOR 
x2 :: String->String->String
x2 str1 str2 = zipWith xor str1 str2
-- x0r fin
-- hexaconversion start 
binarytohexa8 :: String->String
binarytohexa8 [] = []
binarytohexa8 (a:b:c:d:xs) = converthexa8 ([a]++[b] ++[c] ++[d]) ++ binarytohexa8 xs
converthexa8 :: String -> String 
converthexa8 a 
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
 -- hexa finished
 -- final encrypted text
divide1 :: String->String->String
divide1 abce k3 = (encrypt xs sx k4 1)
 where 
       xs = take 32 $ subkey5 (binarystring abce)
       sx = drop 32 $ subkey5 (binarystring abce)
       k4 = binarystring k3
encrypt :: String->String->String->Int->String
encrypt xs sx k4 0 = sx ++ xs
encrypt xs sx k4 n = encrypt sx (function sx (key k4 1)) k4 (n-1)
--final permutation start
ge :: String->Int->Char
ge abs l = abs !! (j-1)
 where j = get_index0 l
get_index0 :: Int -> Int
get_index0 i = [40,8,48,16,56,24,64,32,39,7,47,15,55,23,63,31,38,6,46,14,54,22,62,30,37,5,45,13,53,21,61,29,36,4,44,12,52,20,60,28,35,3,43,11,51,19,59,27,34,2,42,10,50,18,58,26,33,1,41,9,49,17,57,25]!!(i-1)


subkeyfinal :: String->String
subkeyfinal key = [ge key 1]++ [ge key 2] ++ [ge key 3] ++ [ge key 4]++[ge key 5] ++ [ge key 6] ++ [ge key 7] ++ [ge key 8]++[ge key 9] ++ [ge key 10] ++ [ge key 11] ++ [ge key 12]++[ge key 13] ++ [ge key 14] ++ [ge key 15] ++ [ge key 16] ++ [ge key 17] ++ [ge key 18] ++ [ge key 19]++[ge key 20] ++ [ge key 21] ++ [ge key 22] ++ [ge key 23]++[ge key 24] ++ [ge key 25] ++ [ge key 26] ++ [ge key 27]++[ge key 28] ++ [ge key 29] ++ [ge key 30] ++[ge key 31]++[ge key 32] ++ [ge key 33]++[ge key 34] ++ [ge key 35] ++ [ge key 36] ++ [ge key 37]++[ge key 38] ++ [ge key 39] ++ [ge key 40] ++ [ge key 41] ++ [ge key 42] ++ [ge key 43] ++ [ge key 44]++[ge key 45] ++ [ge key 46] ++ [ge key 47] ++ [ge key 48] ++[ge key 49]++[ge key 50] ++[ge key 51] ++ [ge key 52 ] ++ [ge key 53 ] ++ [ge key 54]++[ge key 55]++[ge key 56] ++ [ge key 57]++ [ge key 58]++[ge key 59]++ [ge key 60] ++ [ge key 61]++ [ge key 62 ] ++ [ge key 63] ++[ge key 64]

-- final permutation finisgh

-- p1 permutation of input text       
getchar5 :: String->Int->Char
getchar5 abs l = abs !! (j-1)
 where j = get_index6 l
get_index6 :: Int -> Int
get_index6 i = [58,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,62,54,46,38,30,22,14,6,64,56,48,40,32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7] !! (i-1)

subkey5 :: String->String
subkey5 key = [getchar5 key 1]++ [getchar5 key 2] ++ [getchar5 key 3] ++ [getchar5 key 4]++[getchar5 key 5] ++ [getchar5 key 6] ++ [getchar5 key 7] ++ [getchar5 key 8]++[getchar5 key 9] ++ [getchar5 key 10] ++ [getchar5 key 11] ++ [getchar5 key 12]++[getchar5 key 13] ++ [getchar5 key 14] ++ [getchar5 key 15] ++ [getchar5 key 16] ++ [getchar5 key 17] ++ [getchar5 key 18] ++ [getchar5 key 19]++[getchar5 key 20] ++ [getchar5 key 21] ++ [getchar5 key 22] ++ [getchar5 key 23]++[getchar5 key 24] ++ [getchar5 key 25] ++ [getchar5 key 26] ++ [getchar5 key 27]++[getchar5 key 28] ++ [getchar5 key 29] ++ [getchar5 key 30] ++[getchar5 key 31]++[getchar5 key 32] ++ [getchar5 key 33]++[getchar5 key 34] ++ [getchar5 key 35] ++ [getchar5 key 36] ++ [getchar5 key 37]++[getchar5 key 38] ++ [getchar5 key 39] ++ [getchar5 key 40] ++ [getchar5 key 41] ++ [getchar5 key 42] ++ [getchar5 key 43] ++ [getchar5 key 44]++[getchar5 key 45] ++ [getchar5 key 46] ++ [getchar5 key 47] ++ [getchar5 key 48] ++[getchar5 key 49]++[getchar5 key 50] ++[getchar5 key 51] ++ [getchar5 key 52 ] ++ [getchar5 key 53 ] ++ [getchar5 key 54]++[getchar5 key 55]++[getchar5 key 56] ++ [getchar5 key 57]++ [getchar5 key 58]++[getchar5 key 59]++ [getchar5 key 60] ++ [getchar5 key 61]++ [getchar5 key 62 ] ++ [getchar5 key 63] ++[getchar5 key 64]
--final permutation finish
binarystring  :: String -> String
binarystring [] = []
binarystring (x:xs)   = convert_binary2 x ++ binarystring xs
convert_binary2 :: Char -> String
convert_binary2 a 
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

getchar :: String->Int -> Char
getchar abs l = abs !! (j-1)
 where j = get_index3 l
get_index3 :: Int -> Int
get_index3 i = [57,49,41,33,25,17,9,1,58,50,42,34,26,18,10,2,59,51,43,35,27,19,11,3,60,52,44,36,63,55,47,39,31,23,15,7,6,54,46,8,30,22,14,6,61,53,45,37,29,21 ,13,5,28,20 ,12,4] !! (i-1)
subkey :: String->String
subkey key = [getchar key 1] ++ [getchar key 2] ++ [getchar key 3] ++ [getchar key 4]++[getchar key 5] ++ [getchar key 6] ++ [getchar key 7] ++ [getchar key 8]++[getchar key 9] ++ [getchar key 10] ++ [getchar key 11] ++ [getchar key 12]++[getchar key 13] ++ [getchar key 14] ++ [getchar key 15] ++ [getchar key 16] ++ [getchar key 17] ++ [getchar key 18] ++ [getchar key 19]++[getchar key 20] ++ [getchar key 21] ++ [getchar key 22] ++ [getchar key 23]++[getchar key 24] ++ [getchar key 25] ++ [getchar key 26] ++ [getchar key 27]++[getchar key 28] ++ [getchar key 29] ++ [getchar key 30] ++[getchar key 31]++[getchar key 32]++[getchar key 33]++[getchar key 34] ++ [getchar key 35] ++ [getchar key 36] ++ [getchar key 37]++[getchar key 38] ++ [getchar key 39] ++ [getchar key 40] ++ [getchar key 41] ++ [getchar key 42] ++ [getchar key 43] ++ [getchar key 44]++[getchar key 45] ++ [getchar key 46] ++ [getchar key 47] ++ [getchar key 48]++[getchar key 49] ++ [getchar key 50] ++ [getchar key 51] ++ [getchar key 52]++[getchar key 53] ++ [getchar key 54] ++ [getchar key 55] ++ [getchar key 56] 
  -- convert 56 bit key into 48 key
getchar3 :: String->Int -> Char
getchar3 abs l = abs !! (j-1)
 where j = get_index2 l
get_index2 :: Int -> Int
get_index2 i = [14,17,11,24,1,5,3,28,15,6,21,10,23,19,12,4,26,8,16,7,27, 20,13,2,41,52,31,37,47,55,30,40, 51,45,33,48,44,49,39,56,34,53,46,42,50,36,29,32] !! (i-1)

bit :: String->String
bit key = [getchar3 key 1] ++ [getchar3 key 2] ++ [getchar3 key 3] ++ [getchar3 key 4]++[getchar3 key 5] ++ [getchar3 key 6] ++ [getchar3 key 7] ++ [getchar3 key 8]++[getchar3 key 9] ++ [getchar3 key 10] ++ [getchar3 key 11] ++ [getchar3 key 12]++[getchar3 key 13] ++ [getchar3 key 14] ++ [getchar3 key 15] ++ [getchar3 key 16] ++ [getchar3 key 17] ++ [getchar3 key 18] ++ [getchar3 key 19]++[getchar3 key 20] ++ [getchar3 key 21] ++ [getchar3 key 22] ++ [getchar3 key 23]++[getchar3 key 24] ++ [getchar3 key 25] ++ [getchar3 key 26] ++ [getchar3 key 27]++[getchar3 key 28] ++ [getchar3 key 29] ++ [getchar3 key 30] ++[getchar3 key 31]++[getchar3 key 32]++[getchar3 key 33]++[getchar3 key 34] ++ [getchar3 key 35] ++ [getchar3 key 36] ++ [getchar3 key 37]++[getchar3 key 38] ++ [getchar3 key 39] ++ [getchar3 key 40] ++ [getchar3 key 41] ++ [getchar3 key 42] ++ [getchar3 key 43] ++ [getchar3 key 44]++[getchar3 key 45] ++ [getchar3 key 46] ++ [getchar3 key 47]++[getchar3 key 48]

   

-- 16 subkey are generated through this function
key :: String ->Int->String
key abc p 
 | p == 1 = bit b1
 | p == 2 = bit b2
 | p == 3 = bit b3
 | p == 4 = bit b4
 | p == 5 = bit b5
 | p == 6 = bit b6
 | p == 7 = bit b7
 | p == 8 = bit b8
 | p == 9 = bit b9
 | p == 10 = bit b10
 | p == 11 = bit b11
 | p == 12 = bit b12
 | p == 13 = bit b13
 | p == 14 = bit b14
 | p == 15 = bit b15
 | p == 16 = bit b16
 | otherwise = []
 where b0 =  subkey abc
       b1 = strtostr (take 28 b0) 1 ++ strtostr (reverse (take 28 (reverse b0))) 1 
       b2 = strtostr (take 28 b1) 2 ++ strtostr (reverse (take 28 (reverse b1))) 2
       b3 = strtostr (take 28 b2) 3 ++ strtostr (reverse (take 28 (reverse b2))) 3 
       b4 = strtostr (take 28 b3) 4 ++ strtostr (reverse (take 28 (reverse b3))) 4
       b5 = strtostr (take 28 b4) 5 ++ strtostr (reverse (take 28 (reverse b4))) 5 
       b6 = strtostr (take 28 b5) 6 ++ strtostr (reverse (take 28 (reverse b5))) 6
       b7 = strtostr (take 28 b6) 7 ++ strtostr (reverse (take 28 (reverse b6))) 7 
       b8 = strtostr (take 28 b7) 8 ++ strtostr (reverse (take 28 (reverse b7))) 8
       b9 = strtostr (take 28 b8) 9 ++ strtostr (reverse (take 28 (reverse b8))) 9
       b10 = strtostr (take 28 b9) 10 ++ strtostr (reverse (take 28 (reverse b9))) 10
       b11 = strtostr (take 28 b10) 11 ++ strtostr (reverse (take 28 (reverse b10))) 11
       b12 = strtostr (take 28 b11) 12 ++ strtostr (reverse (take 28 (reverse b11))) 12
       b13 = strtostr (take 28 b12) 13 ++ strtostr (reverse (take 28 (reverse b12))) 13 
       b14 = strtostr (take 28 b13) 14 ++ strtostr (reverse (take 28 (reverse b13))) 14
       b15 = strtostr (take 28 b14) 15 ++ strtostr (reverse (take 28 (reverse b14))) 15
       b16 = strtostr (take 28 b15) 16 ++ strtostr (reverse (take 28 (reverse b15))) 16
-- initial rotaion of left and right part of keys
strtostr :: String->Int->String
strtostr str1 k
 | k==1||k==2||k ==9 ||k==16 = reverse (take (27) (reverse str1)) ++ take 1 (str1)
 | otherwise = reverse (take (26) (reverse str1))++take 2 (str1)  
  -- key part finishes here
-- function starts here   
index_to_index ::Int->Int->Int->Int
index_to_index p i j 
 | p == 1 = [14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7,0,15,7,4,1,14,2,13,1,10,6,12,11,9,5,3,8,4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0,15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13] !! (i*16 + j)
 | p == 2 = [15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10,3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5,0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15,13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9] !! (i*16 + j)
 | p == 3 = [10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8,13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1,13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7,1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12]!!(i*16 + j)
 | p == 4 = [7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15,13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9,10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4,3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14] !!(i*16 + j)
 | p == 5 = [2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9,14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6,4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14,11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3] !!(i*16 + j)
 | p == 6 = [12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11,10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8,9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6,4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13]!!(i*16 + j)
 | p == 7 = [4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1,13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6,1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2,6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12]!!(i*16 + j)
 | p == 8 = [13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7,1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2,7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8,2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11]!!(i*16 + j) 
 | otherwise = 56



second_part :: String->Int
second_part [] = 56
second_part (a:b:c:d:e:f:xs) = binary_to_decimal ([b] ++ [c] ++ [d] ++ [e])


first_part :: String->Int
first_part [] = 56
first_part (a:b:c:d:e:f:xs) = binary_to_decimal ([a] ++ [f])
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
 


  
timepas :: String -> Int -> Int
timepas abc m 
 | abc == "" = 0
 | otherwise = (index_to_index m a b)
   where a = (first_part abc)
         b = (second_part abc)

convert_binary :: Int-> String
convert_binary k 
 | k == 0 = "0000"
 | k == 1 = "0001"
 | k == 2 = "0010"
 | k == 3 = "0011"
 | k == 4 = "0100"
 | k == 5 = "0101"
 | k == 6 = "0110"
 | k == 7 = "0111"
 | k == 8 = "1000"
 | k == 9 = "1001"
 | k == 10 = "1010" 
 | k == 11 = "1011"
 | k == 12 = "1100"
 | k == 13 = "1101"
 | k == 14 = "1110"
 | k == 15 = "1111"
 | otherwise = []       
s_box :: Int-> String ->String
s_box n hgi  = convert_binary b 
 where b = (timepas hgi n)

divide :: String-> String
divide abcd = b1 ++ b2 ++ b3 ++ b4 ++ b5 ++ b6 ++ b7 ++ b8
 where b1 = s_box 1 (take 6 abcd)
       b2 = s_box 2 (take 6 (reverse (take 42 (reverse abcd))))
       b3 = s_box 3 (take 6 (reverse (take 36 (reverse abcd)))) 
       b4 = s_box 4 (take 6 (reverse (take 30 (reverse abcd))))
       b5 = s_box 5 (take 6 (reverse (take 24 (reverse abcd))))
       b6 = s_box 6 (take 6 (reverse (take 18 (reverse abcd))))
       b7 = s_box 7 (take 6 (reverse (take 12 (reverse abcd))))
       b8 = s_box 8 (take 6 (reverse (take 6  (reverse abcd))))
       

subkey7 :: String->String
subkey7 key = [getchar9 key 1] ++ [getchar9 key 2] ++ [getchar9 key 3] ++ [getchar9 key 4]++[getchar9 key 5] ++ [getchar9 key 6] ++ [getchar9 key 7] ++ [getchar9 key 8]++[getchar9 key 9] ++ [getchar9 key 10] ++ [getchar9 key 11] ++ [getchar9 key 12]++[getchar9 key 13] ++ [getchar9 key 14] ++ [getchar9 key 15] ++ [getchar9 key 16] ++ [getchar9 key 17] ++ [getchar9 key 18] ++ [getchar9 key 19]++[getchar9 key 20] ++ [getchar9 key 21] ++ [getchar9 key 22] ++ [getchar9 key 23]++[getchar9 key 24] ++ [getchar9 key 25] ++ [getchar9 key 26] ++ [getchar9 key 27]++[getchar9 key 28] ++ [getchar9 key 29] ++ [getchar9 key 30] ++ [getchar9 key 31] ++[getchar9 key 32]   


getchar9 :: String->Int -> Char
getchar9 abs l = abs !! (j-1)
 where j = get_index9 l
get_index9 :: Int -> Int
get_index9 i = [16,7,20,21,29,12,28,17,1,15,23,26,5,18,31,10,2,8,24,14,32,27,3,9,19,13,30,6,22,11,4,25] !! (i-1)    
final_sbox :: String->String
final_sbox ab = subkey7 (divide ab)

function :: String->String->String
function r k = final_sbox s 
 where s = (x2 (enot r 8) k)
      
xor :: Char->Char->Char
xor a b
 | a==b && a=='0' = '0'
 | a==b && a=='1' = '0'
 | otherwise = '1'
-- EXPAND 32 TO 48 
etchar :: String->Int -> Char
etchar abs l = abs !! (j-1)
 where j = get_index1 l
get_index1 :: Int -> Int
get_index1 i = [32,1,2,3,4,5,4,5,6,7,8,9,8,9,10,11,12,13,12,13,14,15,16,17,16,17,18,19,20,21,20,21,22,23,24,25,24,25,26,27,28,29,28,29,30,31,32,1]!!(i-1)
bit1 :: String->String
bit1 key = [etchar key 1] ++ [etchar key 2] ++ [etchar key 3] ++ [etchar key 4]++[etchar key 5] ++ [etchar key 6] ++ [etchar key 7] ++ [etchar key 8]++[etchar key 9] ++ [etchar key 10] ++ [etchar key 11] ++ [etchar key 12]++[etchar key 13] ++ [etchar key 14] ++ [etchar key 15] ++ [etchar key 16] ++ [etchar key 17] ++ [etchar key 18] ++ [etchar key 19]++[etchar key 20] ++ [etchar key 21] ++ [etchar key 22] ++ [etchar key 23]++[etchar key 24] ++ [etchar key 25] ++ [etchar key 26] ++ [etchar key 27]++[etchar key 28] ++ [etchar key 29] ++ [etchar key 30] ++[etchar key 31]++[etchar key 32]++[etchar key 33]++[etchar key 34] ++ [etchar key 35] ++ [etchar key 36] ++ [etchar key 37]++[etchar key 38] ++ [etchar key 39] ++ [etchar key 40] ++ [etchar key 41] ++ [etchar key 42] ++ [etchar key 43] ++ [etchar key 44]++[etchar key 45] ++[etchar key 46] ++ [etchar key 47] ++ [etchar key 48]
-- this is to take input in hexa form
split :: String->Int->[String]
split _ 0 = []
split (a:b:c:d:xs) n = (a:b:c:d:[]):split xs (n-1)

process :: [Char] -> [Char]
process (x:xs) = let some = if (x == '1') then '0' else '1' in ([some] ++ [x] ++ xs ++ [some])

enot :: String -> Int -> [Char]
enot list n = ls>>= process
 where ls = split list n 



