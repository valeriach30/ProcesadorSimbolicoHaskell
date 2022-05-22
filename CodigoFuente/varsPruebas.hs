import Sintax
import Vars
main :: IO ()  
main = do
    let pru1 = Variable "a" .&& Variable "b"
        pru2 = Variable "x" .&& Variable "x"
    print(vars(pru1))
    print(vars(pru2))  