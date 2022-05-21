import Gen_bools
main :: IO ()    
main = do 
    putStr("Generar booleanos de 2: ")
    print(gen_bools 2)
    putStr("\nGenerar booleanos de 4: ")
    print(gen_bools 4)