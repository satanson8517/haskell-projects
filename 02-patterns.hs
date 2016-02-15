    sayMe :: (Integral a) => a -> String  
    sayMe 1 = "Jedna!"  
    sayMe 2 = "Dva!"  
    sayMe 3 = "Tři!"  
    sayMe 4 = "Čtyři!"  
    sayMe 5 = "Pět!"  
    sayMe x = "Není mezi 1 a 5."  

    factorial :: (Integral a) => a -> a  
    factorial 0 = 1  
    factorial n = n * factorial (n - 1)  

    first :: (a, b, c) -> a  
    first (x, _, _) = x  

    second :: (a, b, c) -> b  
    second (_, y, _) = y  

    third :: (a, b, c) -> c  
    third (_, _, z) = z  

    head' :: [a] -> a  
    head' [] = error "Nemůžeš zjistit první prvek prázdného seznamu, hňupe!"  
    head' (x:_) = x 

    tell :: (Show a) => [a] -> String  
    tell [] = "Seznam je prázdný."  
    tell (x:[]) = "Seznam obsahuje jeden prvek: " ++ show x  
    tell (x:y:[]) = "Seznam obsahuje dva prvky: " ++ show x ++ " a " ++ show y  
    tell (x:y:_) = "Seznam je dlouhý. První dva prvky jsou: " ++ show x ++ " a " ++ show y 

    capital :: String -> String  
    capital "" = "Prázdný řetězec, jejda!"  
    capital all@(x:xs) = "První písmeno řetězce " ++ all ++ " je " ++ [x]  

    bmiTell :: (RealFloat a) => a -> String  
    bmiTell bmi  
    	| bmi <= 18.5 = "Jsi podvyživený, ty emo, ty!"  
    	| bmi <= 25.0 = "Jsi údajně normální. Pche, vsadím se, že jsi šereda!"  
    	| bmi <= 30.0 = "Jsi tlustý! Zhubni, špekoune!"  
    	| otherwise   = "Jsi velryba, gratuluji!" 
