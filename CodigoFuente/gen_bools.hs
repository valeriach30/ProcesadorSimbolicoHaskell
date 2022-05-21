module Gen_bools where
    gen_bools 0 = [[]]
    gen_bools n = (map(True :) anterior) ++ (map (False :) anterior)
                where anterior = gen_bools(n-1)
