module EvalProp where
    import Control.Exception  
    
    -- Para correr en VSCODE: agregar la sintaxis a este archivo 
    -- y comentar la linea donde se importa Sintax
    -- Importar modulo sintaxis
    import Sintax

    data Identificador = String;

    -- Lista de tuplas
    type Ambiente a b = [(Identificador,b)]  

    {-
    Funcion que devuelve el valor de la llave que recibe 
    si es encontrado.
    - Input: LLave
    - Output:Valor de la llave
    -}
    busca :: (Eq x) => x -> [(x,y)] -> y
    busca _ [] = error "Elemento no encontrado" 
    busca z ((x,y):xs) = if z == x then y else busca z xs 

    {-
    Funcion que evalua una proposicion segun un ambiente
    - Input: Un ambiente y una proposicion
    - Output: El valor booleano resultante de la evaluacion
    -}
    evalProp ambiente prop = 
        case prop of 
        Constante valor -> valor
        Variable var -> busca var ambiente
        Negacion prop1 -> not (evalProp ambiente prop1)
        Conjuncion prop1 prop2 ->
            let valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
            in  valor1 && valor2
        Disyuncion prop1 prop2 ->
            let valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
            in  valor1 || valor2
        Implicacion prop1 prop2 ->
            let valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
            in
                case (valor1, valor2) of
                    (True, False) -> False  
                    _ -> True         
        Equivalencia prop1 prop2 ->
            let valor1 = evalProp ambiente prop1
                valor2 = evalProp ambiente prop2
            in valor1 == valor2   

    main :: IO ()  
    main = do
        -- prueba busca 
        print(busca "a" [("a", True), ("b", True)])
        -- prubea evalProp
        let vp = Variable "p" 
            vq = Variable "q" 
            vr = Variable "r" 
            prop1 = vp .&& vq .=> vq .|| vp .=> vp .&& vp
            amb1 = [("p", True), ("q", False)]
        print(evalProp amb1 prop1)