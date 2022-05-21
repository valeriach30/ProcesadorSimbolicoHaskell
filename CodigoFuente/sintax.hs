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