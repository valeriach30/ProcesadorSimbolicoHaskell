import Sintax
import FNC
main :: IO ()  
main = do
    let prufnd1 = (.~)(Variable "p") .|| (Variable "q") 
        prufnd2 = (.~)(Variable "q") .&& (Variable "p")
        prufnd3 = prufnd1 .=> prufnd2
    fnc(prufnd3)