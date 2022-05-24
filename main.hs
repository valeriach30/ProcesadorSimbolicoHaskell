module Progra where

---------------------------- 1 VARS ----------------------------

-- https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
groupBy :: String -> Char -> [String] -> [String]
groupBy "" _ [] = []
groupBy (x:xs) c []
    | x == c = groupBy xs c []
    | otherwise = groupBy xs c [[x]]
groupBy "" _ (r:rs) = reverse result
    where result = if r == "" then rs else r : rs
groupBy (x:xs) c (r:rs)
    | x == c = groupBy xs c $ nextGroup ++ rs
    | otherwise = groupBy xs c $ (r ++ [x]) : rs
    where nextGroup = if r == "" then [r] else "" : [r]

isLetter l = l `elem` ['A'..'Z'] || l `elem` ['a'..'z']
coms s = [ if not (isLetter c) then ',' else c | c <- s ]


rep l h
  | null l = []
  | head l `elem` tail l = rep (tail l) (head (tail l))
  | otherwise = head l: rep (tail l) (head (tail l))

rep0 l = rep l (head l)

vars s = rep0(groupBy (coms s) ',' [])

---------------------------- 2 GEN_BOOLS ----------------------------

gen_bools_aux lista listaR = if length lista == 1 then listaR
                             else do
                                 let x = [rec ++ [cur] | rec <- listaR, cur <- [True, False]]
                                 gen_bools_aux (tail lista) x

gen_bools lista = gen_bools_aux lista [[True], [False]]


-------------------------- 3 AS_VALS ----------------------------
as_vals lista booleanos = if length lista /= length booleanos then [] else zip lista booleanos

---------------------------- 4 AS_VALS ----------------------------
-- Para hacer este codigo se tomo referencia de: 
-- - https://tecdigital.tec.ac.cr/revistamatematica/ContribucionesV7_n2_2006/Parseador/node3.html
-- - https://www.infor.uva.es/~cvaca/asigs/AlgInfPost.htm

-- Transforma el literal "True" o "False" a tipo booleano
obtener_atomo prop = if prop == "True" then True else False

-- Permite aplicar la operacion entre dos proposiciones
evaluar_prop val1 val2 operador = if operador == "/\\" then val1 && val2
                                  else if operador == "\\/" then val1 || val2
                                  else if operador == "=>" then (not val1) || val2
                                  else if operador == "<=>" then do 
                                      let a = evaluar_prop val1 val2 "=>"
                                      let b = evaluar_prop val2 val1 "=>"
                                      a && b
                                  else if operador == "~" then not val1
                                  else False

-- Determina la importancia de la operacion
precedencia elemento = if elemento == "~" then 6
                       else if elemento == "/\\" then 5
                       else if elemento == "\\/" then 4
                       else if elemento == "<=>" then 2
                       else if elemento == "=>" then 3
                       else 1

-- Permite manejar el caso cuando al convertir la expresion se encuentra un "("
par_der pila salida = if length pila == 0 then [pila, salida]
                      else if (pila !! (length pila - 1)) == "(" then [(init pila), salida]
                      else do
                          let newSalida = salida ++ [pila !! (length pila - 1)]
                          let newPila = init pila

                          par_der newPila newSalida

-- Permite manejar el caso cuando al convertir la expresion se encuentra un operador
operador pila salida ope = if length pila == 0 then [pila, salida]
                           else if (precedencia ope) > (precedencia (pila !! (length pila - 1))) then [pila, salida]
                           else do
                               let elemento = (pila !! (length pila - 1))
                               let newSalida = salida ++ [elemento]
                               let newPila = init pila

                               operador newPila newSalida ope

-- Primera parte para obtener el orden necesario de la expresion postfija
a_postfija_aux lista pila salida = if length lista == 0 then [pila, salida]
                                 else do
                                     let elemento = lista !! 0
                                    
                                     --if (precedencia elemento) <= 1 then do
                                     if elemento == "(" then do
                                         let newPila = pila ++ [elemento]
                                         a_postfija_aux (tail lista) newPila salida
                                     else if elemento == ")" then do
                                         let resultados = par_der pila salida

                                         a_postfija_aux (tail lista) (resultados !! 0) (resultados !! 1)
                                     else if precedencia elemento == 1 then do
                                         let newSalida = salida ++ [elemento]
                                         a_postfija_aux (tail lista) pila newSalida
                                     else do
                                         let resultados_ope = operador pila salida elemento
                                         let cPila = resultados_ope !! 0
                                         let newPila = cPila ++ [elemento]

                                         a_postfija_aux (tail lista) newPila (resultados_ope !! 1)

-- Segunda parte para obtener la expresion postfija, termina de vaciar la pila
vaciar_pila lista pila = if length pila == 0 then lista
                         else do
                             let newLista = lista ++ [pila !! (length pila -1)]

                             vaciar_pila newLista (init pila)

-- Convierte una expresion a postfija
a_postfija lista = if length lista >= 0 then do
                                            let resultados = a_postfija_aux lista [] []

                                            vaciar_pila (resultados !! 1) (resultados !! 0)
                   else []

-- Auxiliar que permite evaluar la expresion postfija
eval_postfija_aux lista pila = if length lista == 0 then pila !! 0 
                               else if precedencia (lista !! 0) > 1 then do
                                   if precedencia (lista !! 0) == 6 then do
                                       let elem = pila !! (length pila - 1) 
                                       let result = evaluar_prop elem False (lista !! 0)
                                       let newPila = (init pila) ++ [result]
                                       eval_postfija_aux (tail lista) newPila
                                   else do
                                       let elem = pila !! (length pila - 1) 
                                       let cPila = (init pila)
                                       let elem2 = cPila !! (length cPila - 1) 
                                       let result = evaluar_prop elem2 elem (lista !! 0)
                                       let newPila = (init cPila) ++ [result]
                                       eval_postfija_aux (tail lista) newPila
                               else do
                                   let elem = lista !! 0
                                   let newPila = pila ++ [(obtener_atomo elem)]
                                   eval_postfija_aux (tail lista) newPila

-- Permite evaluar la expresion postfija
eval_postfija lista = eval_postfija_aux lista []

-- Permite evaluar la expresion postfija
evalProp lista = eval_postfija_aux (a_postfija lista) []

---------------------------- 5 TAUT ----------------------------

isP c = c == '(' || c == ')'
rmSpace prop = [ if c == ' ' then ',' else c | c <- prop]

-- last = true if letter
div_params prop last = if length prop == 0 then []
                        else if last && (isLetter (head prop)) then do (head prop) : (div_params (tail prop) True)
                        else if last && not (isLetter (head prop)) then do ',': ((head prop) : (div_params (tail prop) False))
                        else if not last && not (isLetter (head prop)) then do
                            if isP (head prop) then
                             ',': (  ( (head prop) : ",") ++ (div_params (tail prop) False))
                            else do ((head prop) : (div_params (tail prop) False))
                        else  ',': ((head prop) : (div_params (tail prop) True))



toList prop = groupBy (rmSpace (div_params prop True)) ',' []

toSt bo = if bo then "True" else "False"

replace list vals = if length vals == 0 then list
                    else
                        let h = (head vals) in replace [ if l == (fst h) then toSt (snd h) else l | l <- list ] (tail vals)

-- v exprecion entrada 
--let v = "AT\\/ ZR || (al=>x)"
--  replace ( toList v ) (as_vals (vars v) [True, False, False, True])

getCombinations prop = let vars' = vars prop; comb = gen_bools (vars prop); list = toList prop in [ (replace list  (as_vals vars' v)) | v <- comb]

validate ls = if length ls == 0 then True
            else if evalProp (head ls) then validate (tail ls)
            else False

-- [t, t] = t, [t, f] = t, [f, t] = f, [f, f] = t

taut prop = if validate (getCombinations prop) then "es una tautologia" else "no es una tautologia"


---------------------------- 6 FND ----------------------------

find_prop_aux lista pila = if length lista == 0 then pila !! 0 
                               else if precedencia (lista !! 0) > 1 then do
                                   let elem = pila !! (length pila - 1) 
                                   let cPila = (init pila)
                                   let elem2 = cPila !! (length cPila - 1) 
                                   if precedencia (lista !! 0) == 6 then do
                                       let elem = pila !! (length pila - 1) 
                                       let result = (lista !! 0) ++ elem
                                       let newPila = (init pila) ++ [result]
                                       find_prop_aux (tail lista) newPila
                                    else do 
                                        if head lista == "<=>" then do
                                                                   let result = "(("++ "~"++ elem ++ "\\/" ++ elem2 ++ ")" ++ "/\\"  ++ "("++ elem ++ "\\/" ++  "~" ++ elem2 ++ "))"
                                                                   let newPila = (init cPila) ++ [result]
                                                                   find_prop_aux (tail lista) newPila
                                        else if head lista == "=>"  then do 
                                                                   let result = "(("++ "~"++ elem ++ "\\/" ++ elem2 ++ "))"
                                                                   let newPila = (init cPila) ++ [result]
                                                                   find_prop_aux (tail lista) newPila
                                        else do
                                            let result = "(" ++ elem2 ++ (lista !! 0) ++ elem ++ ")"
                                            let newPila = (init cPila) ++ [result]
                                            find_prop_aux (tail lista) newPila
                               else do
                                   let elem = lista !! 0
                                   let newPila = pila ++ [elem]
                                   find_prop_aux (tail lista) newPila


fnd prop = bonita(find_prop_aux ( a_postfija(toList prop)) [])

---------------------------- 7 BONITA ----------------------------
bonita_aux lista pila = if length lista == 0 then do
                                                 let result = pila !! 0 

                                                 if (result !! 0) == '(' then do
                                                     let r = tail result
                                                     init r
                                                 else result
                               else if precedencia (lista !! 0) > 1 then do
                                   if precedencia (lista !! 0) == 6 then do
                                       let elem = pila !! (length pila - 1) 
                                       let result = (lista !! 0) ++ elem 
                                       let newPila = (init pila) ++ [result]
                                       bonita_aux (tail lista) newPila
                                   else do
                                       let elem = pila !! (length pila - 1) 
                                       let cPila = (init pila)
                                       let elem2 = cPila !! (length cPila - 1) 
                                       let result = "(" ++ elem2 ++ (lista !! 0) ++ elem ++ ")"
                                       let newPila = (init cPila) ++ [result]
                                       bonita_aux (tail lista) newPila
                               else do
                                   let elem = lista !! 0
                                   let newPila = pila ++ [elem]
                                   bonita_aux (tail lista) newPila

bonita prop = bonita_aux ( a_postfija(toList prop)) []
