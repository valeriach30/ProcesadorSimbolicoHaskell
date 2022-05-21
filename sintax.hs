module Sintax where
    data Proposicion = 
        Constante Bool 
        | Variable String  
        | Negacion Proposicion 
        | Conjuncion Proposicion Proposicion 
        | Disyuncion Proposicion Proposicion 
        | Implicacion Proposicion Proposicion 
        | Equivalencia Proposicion Proposicion 
    ;
    imprimir :: Proposicion -> String;

    imprimir prop = 
        case prop of 
        Constante False -> "false"
        Constante True -> "true" 
        Variable nombre -> nombre
        Negacion prop1 -> "negacion (" ++ imprimir  prop1 ++ ")"
        Conjuncion prop1 prop2 -> "conjuncion (" ++ (imprimir prop1) ++ ", " ++ imprimir prop2 ++ ")"
        Disyuncion prop1 prop2 -> "disyuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
        Implicacion prop1 prop2 -> "implicacion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
        Equivalencia prop1 prop2 -> "equivalencia (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
    ;

    (.~) :: Proposicion -> Proposicion
    (.~) = Negacion

    infix 7 .&&
    (.&&) :: Proposicion -> Proposicion -> Proposicion
    (.&&) = Conjuncion

    infix 6 .||
    (.||) :: Proposicion -> Proposicion -> Proposicion
    (.||) = Disyuncion

    infixr 5 .=>
    (.=>) :: Proposicion -> Proposicion -> Proposicion
    (.=>) = Implicacion

    infix 4 .<=>
    (.<=>) :: Proposicion -> Proposicion -> Proposicion
    (.<=>) = Equivalencia

    main :: IO ()    
    main = do
        -- pruebas con variables
        let pru1 = (Variable "a") .<=> (Variable "b")
            pru2 = (Variable "a") .&& (Variable "b")
            pru3 = pru1 .|| pru2
            pru4 = (.~) (Variable "a")
        print(imprimir(pru1))
        print(imprimir(pru2))
        print(imprimir(pru3))
        print(imprimir(pru4))

        -- pruebas con constantes
        let f = Constante False
            t = Constante True
            p = f
            q = t
            prop1 = p .=> q .<=> p .|| q
            prop2 = p .=> q .<=> (.~) q .=> (.~) p
        print(imprimir(prop1))
        print(imprimir(prop2))
        