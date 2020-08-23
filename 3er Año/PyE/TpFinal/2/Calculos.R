library(ggplot2)
n=10

In <- tirarMoneda(n)
Tiradas <- c(1:n)
Dn = variableAleatoria(In)
Xn = cumsum(Dn)

proceso1 <- data.frame(In,Tiradas)
proceso2 <- data.frame(Dn,Tiradas)


mi_df<- data.frame(
  "Proceso1" = In,
  "Proceso2" = Xn
)


simular<-function(n){
  In <- tirarMoneda(n)
  Tiradas <- c(1:n)
  Dn = variableAleatoria(In)
  Xn = cumsum(Dn)
  
  proceso1 <- data.frame(In,Tiradas)
  proceso2 <- data.frame(Dn,Tiradas)
  
  
  mi_df<- data.frame(
    "Proceso1" = In,
    "Proceso2" = Xn
  )
  
  return (mi_df)
  
}

tirarMoneda<-function(n){ #Tirar moneda n veces
  return(sample(0:1,n,replace = TRUE))
}

variableAleatoria<- function(In){
  
  Dn <- c()
  
  for (i in In){
    Dn <- c(Dn,(2 * i) - 1)
  }
  return (Dn)
}




ggplot(data = mi_df) +
  geom_point(mapping = aes(x = proceso1, y = proceso2))




ejercicio2 <- data.frame(
  "In" = In,
  "Dn" = Dn,
  "Sn" = fra_p2
)
write.table(ejercicio2)
write.table(x = ejercicio2, file = ".txt", sep = ",", 
            row.names = FALSE, col.names = TRUE)

