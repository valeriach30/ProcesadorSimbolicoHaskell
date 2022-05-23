module FND where
    import EvalProp
    import AsVals
    import Sintax
    import Vars
    import Gen_bools

    imprimir_vals_FND [] n = ")"
    imprimir_vals_FND ((v,b) : vbs) n = 
        if n > 1 then if b then v ++ ":&&:" ++ imprimir_vals_FND vbs (n-1) else "~:" ++ v  ++ ":&&:" ++ imprimir_vals_FND vbs (n-1)
        else if b then v ++ imprimir_vals_FND vbs (n-1) else "~:" ++ v ++ imprimir_vals_FND vbs (n-1)

    fnd prop =
        let
            variables = vars(prop)
            n = length variables
            lista_combinaciones_booleanas = gen_bools n
            imprimir_fila vars_bools es_verdadero isPrimero =
                if isPrimero == 0 then print(if es_verdadero then " :||: (" ++ imprimir_vals_FND vars_bools n else "") 
                else print(if es_verdadero then "(" ++ imprimir_vals_FND vars_bools n else "")       
            recorrer [] isPrimero = putStr("\n")  
            recorrer (fila : mas_filas) isPrimero = 
                let
                    asociacion = AsVals.as_vals variables fila
                    resultado_fila = EvalProp.evalProp asociacion prop
                in imprimir_fila asociacion resultado_fila isPrimero
                   recorrer mas_filas 0
		in recorrer lista_combinaciones_booleanas 1;