multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

largestDivisible :: (Integral a) => a -> a
largestDivisible y = head (filter p [y,y-1..])  
    where p x = x `mod` 3829 == 0