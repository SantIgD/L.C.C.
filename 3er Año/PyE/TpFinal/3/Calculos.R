

plataActual = plataInicial
ganar = 0.55
perder = 1-ganar
vectorProbabilidad = c(ganar,perder)
trayectoria = c(plataInicial)
termino = FALSE




while (!termino) {
  if(tirada(vectorProbabilidad) == ganar){
    
    plataActual = plataActual + 1
  }
  else{
    plataActual = plataActual - 1
  }
  trayectoria = c(trayectoria,plataActual)
  
  if(plataActual == limiteSuperior){termino = TRUE}
  if(plataActual == limiteInferior){termino = TRUE}
}
Jugadas = c(1:length (trayectoria))

mi_df = data.frame(trayectoria,Jugadas)

library(ggplot2)

ggplot(data = mi_df,aes(Jugadas)) +
  geom_point(mapping = aes(x = Jugadas, y = trayectoria)) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'))+ 
  ggtitle("Proceso  \"ruina del jugador\"") + theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y=trayectoria), colour="darkgreen") +
  ylab("Plata")  # first layer







plataInicial = 50
limiteInferior = 0
limiteSuperior = 100
termino = FALSE
trayectoria = c()
while (!termino) {
  if(sample(1:2,1) == 1){
    
    plataActual = plataActual + 1
  }
  else{
    plataActual = plataActual - 1
  }
  trayectoria = c(trayectoria,plataActual)
  
  if(plataActual == limiteSuperior){termino = TRUE}
  if(plataActual == limiteInferior){termino = TRUE}
}






plataInicial = 50
limiteInferior = 0
limiteSuperior = 100

proceso <-function(plataInicial,S,p){
  limiteInferior = 0
  limiteSuperior = S
  plataActual = plataInicial
  ganar = p
  perder = 1-ganar
  vectorProbabilidad = c(ganar,perder)
  trayectoria = c(plataInicial)
  termino = FALSE
  
  
  while (!termino) {
    if(tirada(vectorProbabilidad) == ganar){
      
      plataActual = plataActual + 1
    }
    else{
      plataActual = plataActual - 1
    }
    trayectoria = c(trayectoria,plataActual)
    
    if(plataActual == limiteSuperior){termino = TRUE}
    if(plataActual == limiteInferior){termino = TRUE}
  }
  
  return (trayectoria[length(trayectoria)] == S)
  
  
}
tirada<-function(probabilidades){
  probabilidades = sort(probabilidades)
  
  intervaloAleatorio = 1000000 # Probabilidades de hasta 2 digitos despues del .
  
  tirada = sample(1:intervaloAleatorio,1)
  posicion = 1
  
  for (i in probabilidades){
    if (posicion == length(probabilidades)){
      return (i)
    }
    if(tirada <= i*1000000){
      return (i) # Devuelvo la probabilidad que ocurrio
    }
    
    posicion = posicion +1
    
  }
}


