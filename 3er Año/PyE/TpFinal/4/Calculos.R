

izquierda = 1
derecha = 0

n = 100000









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