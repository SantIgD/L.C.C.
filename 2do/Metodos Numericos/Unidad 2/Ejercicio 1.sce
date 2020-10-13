function  raices = metodoRobusto_cuadratica_calculoRaices(a,b,c) // a coeficiente de x^2, b coeficiente de x, c coeficiente de 1
    raizN = 0
    raizP = 0
    if (b < 0) then
        raizN= 2*c/(-b+sqrt(b^2-4*a*c))   // Formula (14)
        raizP= (-b+sqrt(b^2-4*a*c))/(2*a)// Formula (7)
    else
        raizN= (-b-sqrt(b^2-4*a*c))/(2*a) // Formula (6)
        raizP= 2*c/(-b-sqrt(b^2-4*a*c)) // Formula (15)
    end
    raices = [raizN raizP]
endfunction
