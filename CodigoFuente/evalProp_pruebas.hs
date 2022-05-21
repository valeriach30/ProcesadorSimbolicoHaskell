import Sintax
import EvalProp
main :: IO ()  
main = do
    -- prueba busca 
    putStr("--------------Prueba funcion busca--------------\n")
    putStr("Variable: 'a'\n")
    putStr("Ambiente: [('a', True), ('b', True)]\n")
    putStr("Resultado: ")
    print(busca "a" [("a", True), ("b", True)])
    -- prubea evalProp
    putStr("--------------Prueba funcion evalProp--------------\n")
    let vp = Variable "p" 
        vq = Variable "q" 
        vr = Variable "r" 
        prop1 = vp .&& vq .=> vq .|| vp .=> vp .&& vp
        amb1 = [("p", True), ("q", False)]
    putStr("Ambiente:")
    print(amb1)
    putStr("Proposicion: ")
    putStr(imprimir(prop1))
    putStr("\nResultado: ")
    print(evalProp amb1 prop1)