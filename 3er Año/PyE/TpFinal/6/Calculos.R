P = matrix(nrow=7,ncol=7)

Xa = c(1/2,5,6)
Xb = c(1/3, 1, 3,6)
Xc = c(1/2,4,6)
Xd = c(1,6)
Xe = c(1/4,1,4,6,7)
Xf = c(1/2,1,2)
Xg = c(1/7,1,2,3,4,5,6,7)



P = cargarMatriz(P)


contadorPaginas = c(1:7)

probabilidad= simular(100)


simular<- function(n){

  contadorPaginas= cerarVector(contadorPaginas)
  
  paginaInicial = sample(1:7,1) # Inicio de manera aleatoria
  print("La pagina inicial es")
  print(paginaInicial)
  paginaActual = paginaInicial
  
  for (i in (1:n)){
    paginaActual = surfear(paginaActual)
    contadorPaginas[paginaActual] = contadorPaginas[paginaActual] + 1 
    print("La pagina actual es")
    print(paginaActual)
    print("Contador")
    print(contadorPaginas)
  }
  return (contadorPaginas/n)
  
}

surfear <- function (x){
  paginaActual = obtenerFila(x)
  proximaPagina = paginaActual[elegirProximaPagina(paginaActual)]
  return (proximaPagina)
}


elegirProximaPagina <-function(x){
  unoEntre = length(x)-1
  return (sample(1:unoEntre,1)+1)
}



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

cerarVector <- function(v){
  for (i in (1:7)){
    v[i] = 0
  }
  return (v)
}
cerarMatriz <- function(P){
  for (i in (1:7)){
    for (j in (1:7)){
      P[i,j] =0
    }
  }
  return (P)
}

obtenerFila <- function(x){
  if (x == 1){
    return (Xa)
  }
  if (x == 2){
    return (Xb)
  }
  if (x == 3){
    return (Xc)
  }
  if (x == 4){
    return (Xd)
  }
  if (x == 5){
    return (Xe)
  }
  if (x == 6){
    return (Xf)
  }
  if (x == 7){
    return (Xg)
  }
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
0.02146667 0.26693333 0.34346667 0.22733333
 0.14080000
