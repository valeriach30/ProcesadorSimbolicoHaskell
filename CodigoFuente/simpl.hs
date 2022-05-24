module Simpl where
    -- Importar sintaxis
    import Sintax

    simpl :: Proposicion -> Proposicion;
    simpl prop = 
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
            -- Evaluar el lado derecho
            case prop2 of
            -- REGLA 4: INVERSOS P ^ ~P <=> F
            Negacion p2 -> if imprimir(prop1) == imprimir(p2) then (Constante False) else simpl prop1 .&& simpl prop2
            -- REGLA 5 y 6: 
            --   NEUTRO P ^ V <=> P 
            --   DOMINACION P ^ F <=> F
            Constante p2 -> if p2 == True then prop1 else (Constante False)
            -- REGLA 7: IDEMPOTENCIA P ^ P <=> P
            Variable p2 -> if imprimir(prop1) == imprimir(prop2) then prop1 else simpl prop1 .&& simpl prop2
            -- REGLA 8, 9 y 10: 
            --   DISTRIBUTIVA P ^ ( Q  v R ) <=>  (P ^  Q) v (P ^ R)
            --   ABSORCION P ^ (P v Q) <=> P
            --   ABSORCION NEGADA P ^ (~P v Q) <=> P ^ Q 
            Disyuncion p2 p3 -> 
                if imprimir(prop1) == imprimir(p2) then prop1  -- absorcion normal
                else 
                    if imprimir((.~)prop1) == imprimir(p2) then prop1 .&& p3 -- absorcion negada
                    else (simpl prop1 .&& simpl p2) .|| (simpl prop1 .&& simpl p3) -- distributiva
            _ -> simpl prop1 .&& simpl prop2
                    
        Disyuncion prop1 prop2 ->
            -- Evaluar el lado derecho
            case prop2 of
            -- REGLA 11: INVERSOS P v ~P <=> T
            Negacion p2 -> if imprimir(prop1) == imprimir(p2) then (Constante True) else simpl prop1 .|| simpl prop2
            -- REGLA 12 y 13: 
            --   NEUTRO P v F <=> P 
            --   DOMINACION P v T <=> V
            Constante p2 -> if p2 == False then prop1 else (Constante True)
            -- REGLA 14: IDEMPOTENCIA P v P <=> P
            Variable p2 -> if imprimir(prop1) == imprimir(prop2) then prop1 else simpl prop1 .|| simpl prop2
            -- REGLA 15, 16 y 17: 
            --   DISTRIBUTIVA P v ( Q ^ R ) <=>  (P v  Q) ^ (P v R)
            --   ABSORCION P v (P ^ Q) <=> P
            --   ABSORCION NEGADA P v (~P ^ Q) <=> P v Q 
            Conjuncion p2 p3 -> 
                if imprimir(prop1) == imprimir(p2) then prop1 -- absorcion normal
                else
                    if imprimir((.~)prop1) == imprimir(p2) then prop1 .|| p3 -- absorcion negada
                    else (simpl prop1 .|| simpl p2) .&& (simpl prop1 .|| simpl p3) -- distributiva 
            _ -> simpl prop1 .|| simpl prop2

        --- REGLA 18: IMPLICACION DISYUNCION P => Q <=> ~P v Q
        Implicacion prop1 prop2 -> (.~)(simpl prop1) .|| (simpl prop2)
        _ -> prop
    ;     