

izquierda = 1
derecha = 0

n = 100000

simular<-function(x){
  n=x
  acumulador = 0
  while(x != 0){
    acumulador = acumulador + laberinto()
    x = x-1
  }
  return (acumulador/n)
}

laberinto <-function(){
  salio = FALSE
  acumulador = 0
  contador = 0
  while (salio == FALSE){
    if(elegirCamino()==izquierda){
      if(salidaRetorno()==izquierda){
        acumulador = acumulador + 10
        salio = TRUE
      }
      else{
        acumulador = acumulador + 20
      }
    }
    else{
      acumulador = acumulador+ 7
    }
  }
  return (acumulador)
}


print (acumulador)
decision <- function(x){
  
  if (x==izquierda){
    return (caminoIzquierdo(salidaRetorno()))
  }
  else{
    return (3 + decision(elegirCamino()))
  }
  
}


caminoIzquierdo <- function(x){
  if (x==izquierda){
    return (2)
  }
  else{
    return (5 + decision(elegirCamino()))
  }
  
}


elegirCamino <- function(){
  return (sample(0:1,1))
}

salidaRetorno <- function(){
  return (sample(0:2,1))
}

esperanza<-function(){
  acumulador=0
    for(i in (c(1:n))){
      acumulador = acumulador + decision(elegirCamino())
    }
  return (acumulador / n)
  
}