library(ggplot2)
lambda = 10
intervaloTiempo=1000
Tiempo = c(1:intervaloTiempo)

trayectoriaMuestral= c()
Xn = c()
tActual = 0
for(i in Tiempo){
  res =rexp(1,10)
  tActual = tActual + rexp(1,10)
  trayectoriaMuestral= c(trayectoriaMuestral, tActual)
  Xn = c(Xn,res )
}
midf = data.frame(trayectoriaMuestral,Tiempo)
hist(Xn,xlab="Xn",prob=T,ylab="",main="")
ggplot(data = midf) +
  geom_point(mapping = aes(x = Tiempo, y = trayectoriaMuestral))

