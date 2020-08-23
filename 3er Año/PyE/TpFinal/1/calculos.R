library(ggplot2)


n = 100 # Cantidad de tiradas

simulacion = simular(n)
graficar(simulacion,"grey","pink","black","Número de tirada (n)","Xn","Número de caras al momento n")
write.table(simulacion,"D:\\Facultad\\LCC\\3er Año\\PyE\\TpFinal\\1\\Número de caras al momento n")
simular <- function(n){
  In <- tirarMoneda(n) # n tiradas de una moneda
  
  Xn<- cumsum(In) # numero de caras al momento n
  
  vectorTiradas <- c(1:n)
  
  return (data.frame(Xn,vectorTiradas))
  
}

graficar <- function(simulacion,fondo,linea,borde,nombreX,nombreY,nombreTitulo,ejex,ejey){
  ggplot(simulacion, aes(x=ejex, y=ejey ))+  # Grafico
    geom_line(colour="red") +
    geom_point( size=2, shape=21, fill=linea, colour="red") +
    theme_minimal()+ ggtitle(nombreTitulo) +
    theme(plot.title = element_text(hjust = 0.5))+ 
    theme(panel.background = element_rect(fill = fondo, colour = borde)) +
    ylab(nombreY) + xlab(nombreX) 
  
}


# P(X=1) = 0,43, E(X) = 1 * 0,43 + 0 * 0,57 = 0,43
#
#X100 = 54=> P(X=1) = 0.43 , E(X) = 
#

# TIRADA SESGADA, si es par salio cruz.

resultado_d <- tirarMonedaSesgada(n)
fac <- cumsum(resultado_d)
simulacion2 = data.frame(fac,c(1:n))
graficar(simulacion2,"grey","pink","black","Número de tirada (n)","Xn","Número de caras al momento n",c(1:n),fac)


#P(X=1) = 0,67, E(X) = 0 * 0.33 + 0,67 * 1 = 0,67 

modulo_2 <- function (x){
  r <- c()
  for (i in x){
    r <- c(r,i %% 2)
  }
  return (r)
}

combinacion<-function (k,n){
  return (factorial(k)/(factorial(n) * factorial(k-n)))
}

P<-function(x){ # Probabilidad de distribucion pascal.
  return (combinacion(x-1,2) * (0.39**3) * (0.61**(x-3)) )
}

E<-function(x){
  esperanza <- 0
  tiradas <- c(3:x)
  for (i in tiradas){
    esperanza <- esperanza + (i *  P(i))
    
  }
  return (esperanza)
}

tirarMoneda<-function(n){ #Tirar moneda n veces
  return(sample(0:1,100,replace = TRUE))
}

tirarMonedaSesgada<- function(n){
  return (sample(0:2,100,  replace =TRUE)%%2)
}


