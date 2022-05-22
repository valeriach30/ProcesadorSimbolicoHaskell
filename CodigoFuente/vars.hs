module Vars where
    import Sintax

    filter p []     = []
    filter p (x:xs) = if p x then x : Vars.filter p xs else Vars.filter p xs

    nub []      = []
    nub (x:xs) = x : (nub (Vars.filter (\y -> x /= y) xs))
    
    vars prop =
        let
            las_vars prop =
                case prop of
                Constante _ -> []
                Variable var -> [var]
                Negacion prop1 -> las_vars prop1
                Conjuncion prop1 prop2 -> 
                    let 
                        vars1 = las_vars prop1
                        vars2 = las_vars prop2
                    in  vars1 ++ vars2
                Disyuncion prop1 prop2 -> 
                    let vars1 = las_vars prop1
                        vars2 = las_vars prop2
                    in  vars1 ++ vars2
                Implicacion prop1 prop2 ->
                    let vars1 = las_vars prop1
                        vars2 = las_vars prop2
                    in  vars1 ++ vars2   
                Equivalencia prop1 prop2 -> 
                    let vars1 = las_vars prop1
                        vars2 = las_vars prop2
                    in  vars1 ++ vars2
        in nub (las_vars prop)