import AsVals
main :: IO ()  
main = do
    let pru1 = as_vals ["p"][True]
        pru2 = as_vals ["a", "b", "p"] [True, False, False]
        pru3 = as_vals ["x", "y"] [True, False]
    print(pru1)
    print(pru2)
    print(pru3)