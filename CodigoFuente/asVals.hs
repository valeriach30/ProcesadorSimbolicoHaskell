module AsVals where
    import Control.Exception
    zipP []        []        = []
    zipP (x : xs) (y : ys) = (x, y) : zipP xs ys
    zipP []        _         = [] 
    zipP _         _         = [] 

    zip []        []        = []
    zip (x : xs) (y : ys) = (x, y) : AsVals.zip xs ys
    zip _         _         = error "listas de longitudes distintas"

    as_vals vars bools = AsVals.zip vars bools

    impr_as_vals []             = ""
    impr_as_vals ((v,b) : vbs)  = "(" ++ v ++ "," ++ (if b then "true" else "false") ++ ") " ++ impr_as_vals vbs
