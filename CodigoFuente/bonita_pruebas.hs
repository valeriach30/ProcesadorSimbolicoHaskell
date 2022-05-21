import Sintax
import Bonita
main :: IO ()  
main = do 
    let vp = Variable "p" 
        vq = Variable "q" 
        vr = Variable "r"
        pru1 = (vp .|| ((.~) vp .&& vq)) .|| (.~) vq
        pru2 = vp .&& (vp .|| vq)
        pru3 = vp .&& vq .=> vq .|| vp
        pru4 = (vp .=> vq) .&& ((.~) vp .=> vr) .<=> (vq .|| vr)
    putStr (bonita pru1)
    putStr (bonita pru2)
    putStr (bonita pru3)
    putStr (bonita pru4)