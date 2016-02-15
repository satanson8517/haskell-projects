doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100  
	then x  
	else x*2 
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "To jsem já, Conan O'Brien!"
length' xs = sum [1 | _ <- xs] 
times x arr = [ arr | arr <- [1..x]]