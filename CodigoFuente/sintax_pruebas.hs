import Sintax
main :: IO ()    
main = do
    putStr("-----------Pruebas con variables-----------\n")
    let pru1 = (Variable "a") .<=> (Variable "b")
        pru2 = (Variable "a") .&& (Variable "b")
        pru3 = pru1 .|| pru2
        pru4 = (.~) (Variable "a")
    print(imprimir(pru1))
    print(imprimir(pru2))
    print(imprimir(pru3))
    print(imprimir(pru4))

    -- pruebas con constantes
    putStr("-----------Pruebas con constantes-----------\n")
    let f = Constante False
        t = Constante True
        p = f
        q = t
        prop1 = p .=> q .<=> p .|| q
        prop2 = p .=> q .<=> (.~) q .=> (.~) p
    print(imprimir(prop1))
    print(imprimir(prop2))
