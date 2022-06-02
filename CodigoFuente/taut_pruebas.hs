import Sintax
import Taut
main :: IO ()  
main = do
    let f = Constante False
        t = Constante True
        vp = Variable "p" 
        vq = Variable "q" 
        vr = Variable "r" 
        prop1 = vp .&& vq .=> vq .|| vp
        prop2 = vp .&& vp .|| vq .&& (.~) vq ;
        prop3 = (.~) vp .&& vp .|| vq .&& (.~) vq 
    print(taut prop1)
    print(taut prop2)
    print(taut prop3)
    