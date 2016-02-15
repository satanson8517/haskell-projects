    bmiTell :: (RealFloat a) => a -> a -> String  
    bmiTell weight height  
        | bmi <= skinny = "Jsi podvyživený, ty emo, ty!"  
        | bmi <= normal = "Jsi údajně normální. Pche, vsadím se, že jsi šereda!"  
        | bmi <= fat    = "Jsi tlustý! Zhubni, špekoune!"  
        | otherwise     = "Jsi velryba, gratuluji!"  
        where bmi = weight / height ^ 2  
              skinny = 18.5  
              normal = 25.0  
              fat    = 30.0  