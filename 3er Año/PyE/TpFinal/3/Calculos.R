
plataInicial = 500
limiteInferior = 0
limiteSuperior = 1000
plataActual = plataInicial
ganar = 0.3
perder = 1-ganar
vectorProbabilidad = c(ganar,perder)
trayectoria = c(plataInicial)
termino = FALSE


while (!termino) {
  while (is.null(lanzamiento)){
    lanzamiento = tirada(c(ganar,perder))
  }
  
  
  if(lanzamiento == ganar){
    plataActual = plataActual + 1
  }
  else{
    plataActual = plataActual - 1
  }
  trayectoria = c(trayectoria,plataActual)
  
  if(plataActual == limiteSuperior){termino = TRUE}
  if(plataActual == limiteInferior){termino = TRUE}
}



if (3 != 0){
  ganar = 30
}





tirada<-function(probabilidades){
  probabilidades = sort(probabilidades)
  
  intervaloAleatorio = 100 # Probabilidades de hasta 2 digitos despues del .
  
  tirada = sample(1:intervaloAleatorio,1)
  posicion = 1
  
  for (i in probabilidades){
    if(tirada <= i*100){
      return (i) # Devuelvo la probabilidad que ocurrio
    }
    if (posicion == length(probabilidades)){
      return (i)
    }
    posicion = posicion +1
    
  }
}

