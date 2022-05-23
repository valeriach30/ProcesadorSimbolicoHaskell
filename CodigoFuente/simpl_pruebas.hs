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
        prop = p .|| ((q .|| neq) .&& r)
        prop2 = p .|| (r .&& (q .|| neq) )
        prop3 = (q .|| neq) .|| (r .&& p)
        prop4 = p .&& ((q .&& neq) .|| r)
    putStr(bonita(simpl prop))
    putStr(bonita(simpl prop2))
    putStr(bonita(simpl prop3))
    putStr(bonita(simpl prop4))
