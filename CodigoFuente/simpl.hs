module Simpl where
    -- Importar sintaxis
    import Sintax

    -- Funcion que hace una sola simplificacion
    unaSimp :: Proposicion -> Proposicion;
    unaSimp prop = 
        case prop of 
        Constante valor -> prop
        Variable var -> prop
        Negacion prop1 -> 
            case prop1 of
            -- REGLA 1: DOBLE NEGACION ~~P <=> P
            Negacion p1 -> p1
            -- REGLA 2: DE MORGAN ~(P ^ Q) <=> ~P v ~Q
            Conjuncion p1 p2 -> (.~)p1 .|| (.~)p2
            -- REGLA 3: DE MORGAN ~(P v Q) <=> ~P ^ ~Q
            Disyuncion p1 p2 -> (.~)p1 .&& (.~)p2
            _ -> prop 
        Conjuncion prop1 prop2 -> 
            -- Evaluar el lado izquierdo
            case prop1 of
            Variable p1 ->
                -- Evaluar el lado derecho
                case prop2 of
                -- REGLA 4: INVERSOS P ^ ~P <=> F
                Negacion p2 -> if p1 == imprimir(p2) then (Constante False) else prop
                -- REGLA 5 y 6: 
                --   NEUTRO P ^ V <=> P 
                --   DOMINACION P ^ F <=> F
                Constante p2 -> if p2 == True then (Variable p1) else (Constante False)
                -- REGLA 7: IDEMPOTENCIA P ^ P <=> P
                Variable p2 -> if p1 == p2 then Variable p1 else prop
                -- REGLA 8 y 9: 
                --   DISTRIBUTIVA P ^ ( Q  v R ) <=>  (P ^  Q) v (P ^ R)
                --   ABSORCION P ^ (P v Q) <=> P
                Disyuncion p2 p3 -> 
                    if p1 == imprimir(p2) then Variable p1 else ((prop1 .&& p2) .|| (prop1 .&& p3))
                _ -> prop 
            _ -> prop
        
        Disyuncion prop1 prop2 ->
            case prop1 of
            Variable p1 ->
                -- Evaluar el lado derecho
                case prop2 of
                -- REGLA 10: INVERSOS P v ~P <=> T
                Negacion p2 -> if p1 == imprimir(p2) then (Constante True) else prop
                -- REGLA 11 y 12: 
                --   NEUTRO P v F <=> P 
                --   DOMINACION P v T <=> V
                Constante p2 -> if p2 == False then (Variable p1) else (Constante True)
                -- REGLA 13: IDEMPOTENCIA P v P <=> P
                Variable p2 -> if p1 == p2 then Variable p1 else prop
                -- REGLA 14 y 15: 
                --   DISTRIBUTIVA P v ( Q ^ R ) <=>  (P v  Q) ^ (P v R)
                --   ABSORCION P v (P ^ Q) <=> P
                Conjuncion p2 p3 -> if p1 == imprimir(p2) then Variable p1 else ((prop1 .|| p2) .&& (prop1 .|| p3))
                _ -> prop
            _ -> prop
        --- REGLA 16: IMPLICACION DISYUNCION P => Q <=> ~P v Q
        Implicacion prop1 prop2 -> (.~) prop1 .|| prop2
        _ -> prop
    ;

    simpl :: Proposicion -> Proposicion;
    simpl prop = 
        let nuevaProp = unaSimp prop
            s1 = imprimir(nuevaProp)
            s2 = imprimir(prop)
        in
            if s1 == s2 then prop 
            else simpl nuevaProp
        
