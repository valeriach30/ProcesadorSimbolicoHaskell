module Bonita where
    import Sintax
    bonita :: Proposicion -> String
    bonita prop = 
        let
            par1 = "("
            par2 = ")"
            rodear prop padre =
                let
                    detrodear prop padre =
                        case prop of
                        Constante _ -> False
                        Variable var -> False
                        Negacion prop -> False
                        Conjuncion p1 p2 -> 
                            case padre of
                            Conjuncion p1 p2 -> False
                            _ -> True
                        Disyuncion prop1 prop2 ->
                            case padre of
                            Disyuncion p1 p2 -> False
                            _ -> True
                        Implicacion prop1 prop2 ->
                            case padre of
                            Implicacion p1 p2 -> False
                            _ -> True
                        Equivalencia prop1 prop2 ->
                            case padre of
                            Equivalencia p1 p2 -> False
                            _ -> True                
                in
                    detrodear prop padre 

            bonitaR prop =
                case prop of
                Constante valor -> if valor == True then "true" else "false"
                Variable var -> var
                Negacion prop1 -> 
                    let p1 = bonitaR prop1
                        simbolo = "~ "
                    in
                        simbolo ++ p1
                Conjuncion prop1 prop2 -> 
                    let r1 = rodear prop1 prop
                        r2 = rodear prop2 prop
                        p1 = bonitaR prop1
                        p2 = bonitaR prop2 
                        simbolo = " && "
                    in
                        if r1 == True then
                            -------Ambos tienen descendentes-------
                            if r2 == True then par1 ++ p1 ++ par2 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Solo la 1ra proposicion tienen descendentes-------
                            else par1 ++ p1 ++ par2 ++ simbolo ++ p2
                        else
                            -------Solo la 2da proposicion tienen descendentes-------
                            if r2 == True then p1 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Ninguna tienen descendentes-------
                            else p1 ++ simbolo ++ p2
                Disyuncion prop1 prop2 ->
                    let r1 = rodear prop1 prop
                        r2 = rodear prop2 prop
                        p1 = bonitaR prop1
                        p2 = bonitaR prop2 
                        simbolo = " || "
                    in
                        if r1 == True then
                            -------Ambos tienen descendentes-------
                            if r2 == True then par1 ++ p1 ++ par2 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Solo la 1ra proposicion tienen descendentes-------
                            else par1 ++ p1 ++ par2 ++ simbolo ++ p2
                        else
                            -------Solo la 2da proposicion tienen descendentes-------
                            if r2 == True then p1 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Ninguna tienen descendentes-------
                            else p1 ++ simbolo ++ p2                
                Implicacion prop1 prop2 ->
                    let r1 = rodear prop1 prop
                        r2 = rodear prop2 prop
                        p1 = bonitaR prop1
                        p2 = bonitaR prop2 
                        simbolo = " => "
                    in
                        if r1 == True then
                            -------Ambos tienen descendentes-------
                            if r2 == True then par1 ++ p1 ++ par2 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Solo la 1ra proposicion tienen descendentes-------
                            else par1 ++ p1 ++ par2 ++ simbolo ++ p2
                        else
                            -------Solo la 2da proposicion tienen descendentes-------
                            if r2 == True then p1 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Ninguna tienen descendentes-------
                            else p1 ++ simbolo ++ p2                
                Equivalencia prop1 prop2 ->
                    let r1 = rodear prop1 prop
                        r2 = rodear prop2 prop
                        p1 = bonitaR prop1
                        p2 = bonitaR prop2 
                        simbolo = " <=> "
                    in
                        if r1 == True then
                            -------Ambos tienen descendentes-------
                            if r2 == True then par1 ++ p1 ++ par2 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Solo la 1ra proposicion tienen descendentes-------
                            else par1 ++ p1 ++ par2 ++ simbolo ++ p2
                        else
                            -------Solo la 2da proposicion tienen descendentes-------
                            if r2 == True then p1 ++ simbolo ++ par1 ++ p2 ++ par2
                            -------Ninguna tienen descendentes-------
                            else p1 ++ simbolo ++ p2        
        in
            "Proposicion: " ++ imprimir prop ++ "\nBonita: " ++ bonitaR prop ++ "\n"