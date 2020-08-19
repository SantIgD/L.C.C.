library(ggplot2)
resultado <- sample(0:1,100,replace = TRUE) # 100 tiradas de una moneda
tiradas <- c(1,100)
frec_Acm <- cumsum(resultado) 
proceso = data.frame(resultado,tiradas)
ggplot(proceso, aes(x=Tiradas, y=Xn ))+  # Grafico
    geom_line(colour="red") +
    geom_point( size=2, shape=21, fill="pink", colour="red") +
    theme_minimal()+ ggtitle("Frecuencia acumulada") +
    theme(plot.title = element_text(hjust = 0.5)) + theme(panel.background = element_rect(fill = 'black', colour = 'pink'))

# P(X=1) = 0,43, E(X) = 1 * 0,43 + 0 * 0,57 = 0,43
#
#
#

# TIRADA SESGADA, si es par salio cruz.

resultado_d <- sample(0:2,100,  replace =TRUE)
resultado_d <- modulo_2(modulo_2(resultado_d))

fac <- cumsum(resultado_d)

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



