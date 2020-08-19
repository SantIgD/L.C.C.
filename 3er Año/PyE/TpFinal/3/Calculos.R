
plataInicial = 50
limiteInferior = 0
limiteSuperior = 100
plataActual = plataInicial
ganar = 0.7
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



for(i in c(1:10))
  print (tirada(c(0.7,0.3)) == ganar)





tirada<-function(probabilidades){
  probabilidades = sort(probabilidades)
  
  intervaloAleatorio = 100 # Probabilidades de hasta 2 digitos despues del .
  
  tirada = sample(1:intervaloAleatorio,1)
  posicion = 1
  
  for (i in probabilidades){
    if (posicion == length(probabilidades)){
      return (i)
    }
    if(tirada <= i*100){
      return (i) # Devuelvo la probabilidad que ocurrio
    }
    
    posicion = posicion +1
    
  }
}

