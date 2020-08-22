P = matrix(nrow=7,ncol=7)

Xa = c(1/2,5,6)
Xb = c(1/3, 1, 3,6)
Xc = c(1/2,4,6)
Xd = c(1,6)
Xe = c(1/4,1,4,6,7)
Xf = c(1/2,1,2)
Xg = c(1/7,1,2,3,4,5,6,7)



P = cargarMatriz(P)

cargarMatriz <- function(P){
  P = cerarMatriz(P)
  indiceFilas = 0

  P = cargarFila(P,Xa,1)
  P = cargarFila(P,Xb,2)
  P = cargarFila(P,Xc,3)
  P = cargarFila(P,Xd,4)
  P = cargarFila(P,Xe,5)
  P = cargarFila(P,Xf,6)
  P = cargarFila(P,Xg,7)
  
  return (P)
}


cerarMatriz <- function(P){
  for (i in (1:7)){
    for (j in (1:7)){
      P[i,j] =0
    }
  }
  return (P)
}

cargarFila <- function(P,x,fila){
  indiceFila = 0
  for (i in x){
    indiceFila = indiceFila + 1
    if (indiceFila != 1){
      P[fila,i] = x[1]
    }
  }
  return (P)
}

