module FNC where
    import EvalProp
    import AsVals
    import Sintax
    import Vars
    import Gen_bools

    imprimir_vals_FNC [] n = ")"
    imprimir_vals_FNC ((v,b) : vbs) n = 
        if n > 1 then if b then "~:" ++ v ++ ":||:" ++ imprimir_vals_FNC vbs (n-1) else v  ++ ":||:" ++ imprimir_vals_FNC vbs (n-1)
        else if b then "~:" ++ v ++ imprimir_vals_FNC vbs (n-1) else v ++ imprimir_vals_FNC vbs (n-1)

    fnc prop =
        let
            variables = vars(prop)
            n = length variables
            lista_combinaciones_booleanas = gen_bools n
            imprimir_fila vars_bools es_verdadero isPrimero =
                if isPrimero == 0 then putStr(if es_verdadero then "" else " :&&: (" ++ imprimir_vals_FNC vars_bools n) 
                else putStr(if es_verdadero then "" else "(" ++ imprimir_vals_FNC vars_bools n)        
            recorrer [] isPrimero = putStr("\n")  
            recorrer (fila : mas_filas) isPrimero = 
                let
                    asociacion = AsVals.as_vals variables fila
                    resultado_fila = EvalProp.evalProp asociacion prop
                in do imprimir_fila asociacion resultado_fila isPrimero
                      recorrer mas_filas 0
        in recorrer lista_combinaciones_booleanas 1