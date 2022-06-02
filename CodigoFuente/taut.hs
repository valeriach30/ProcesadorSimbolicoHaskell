module Taut where
    -- Importar sintaxis
    import Sintax
    import Vars
    import Gen_bools
    import AsVals
    import EvalProp

    taut prop =
        let
            -- variables de trabajo
            variables = vars(prop)
            n = length variables
            lista_combinaciones_booleanas = gen_bools n
            
            -- generar evaluaciones de la proposición
            recorrer [] = "True"
            recorrer (fila : mas_filas) = 
                let
                    -- establecer una asociación entre variables y una combinación de valores booleanos (fila) 
                    asociacion = as_vals variables fila
                    -- esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop 
                    evaluacion_es_verdadera = evalProp asociacion prop
                    filas = mas_filas
                in
                    if evaluacion_es_verdadera == True then
                        recorrer (filas) --podría ser una tautología, continuar evaluando otras combinaciones
                    else
                        let
                            diagnosticar [] = "solo involucra constantes y la proposicion es falsa"
                            diagnosticar asociacion = impr_as_vals asociacion
                        in
                            imprimir (prop) ++ " NO es una tautologia, porque " ++ diagnosticar asociacion
        in
            if recorrer lista_combinaciones_booleanas == "True" then
                imprimir prop ++ " SI es una tautologia"
            else
                recorrer lista_combinaciones_booleanas
                
