



probabilidadRuina <- function(plataInicial,S,p,n){
  

  frecRuina = 0
  for (i in c(1:n)){
    if(!proceso(plataInicial,S,p)){
      frecRuina = frecRuina+1
    }
  }
  return (frecRuina/n)
  
}

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

