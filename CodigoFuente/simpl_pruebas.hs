import Sintax
import Simpl
import Bonita
main :: IO ()  
main = do 
    let p = Variable "p"
        p2 = Variable "p"
        q = Variable "q"
        r = Variable "r"
        f = Constante False
        t = Constante True
        ne = (.~) p
        neq = (.~) q
        -- Para probas inversos y neutro 
        prop = p .|| (r .&& (q .|| neq) ) 
        prop2 = p .&& (r .|| (q .&& neq))
        -- Para probar doble negacion 
        prop3 = (.~)((.~)(q .|| p))
        -- Para probar de morgan
        prop4 = (.~)((q .|| p) .&& (r .|| p))
        -- Para probar dominacion
        prop5 = p .&& f
        -- Para probar distributiva e idemportencia 
        prop6 = p .&& ((q .|| q) .|| (r .&& r))
        -- Para probar abstraccion e inversos
        prop7 = (q .|| (q .&& neq)) .|| p 
        -- Para probar implicacion disyuncion
        prop8 = p .=> (q .=> r) 
        -- Para probar absorcion
        prop9 = (q .|| r) .|| ((q .|| r) .&& p)  
    putStr("\n------Prop1------\n")
    putStr(bonita prop)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop))
    
    putStr("\n\n------Prop2------\n")
    putStr(bonita prop2)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop2))
    
    putStr("\n\n------Prop3------\n")
    putStr(bonita prop3)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop3))
    
    putStr("\n\n------Prop4------\n")
    putStr(bonita prop4)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop4))

    putStr("\n\n------Prop5------\n")
    putStr(bonita prop5)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop5))

    putStr("\n\n------Prop6------\n")
    putStr(bonita prop6)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop6))

    putStr("\n\n------Prop7------\n")
    putStr(bonita prop7)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop7))

    putStr("\n\n------Prop8------\n")
    putStr(bonita prop8)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop8))

    putStr("\n\n------Prop9------\n")
    putStr(bonita prop9)
    putStr("------Simplificacion------\n")
    putStr(bonita(simpl prop9))