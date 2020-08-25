library(ggplot2)
lambda = 10
intervaloTiempo=10000
Tiempo = c(1:intervaloTiempo)
tiempo = c("a","b","c")


trayectoriaMuestral= c()
Xn = c()
tActual = 0
for(i in Tiempo){
  tiempoLLegada =rexp(1,10)
  tActual = tActual + tiempoLLegada
  trayectoriaMuestral= c(trayectoriaMuestral, tActual)
  Xn = c(Xn,tiempoLLegada )
}
midf = data.frame(trayectoriaMuestral,Tiempo)

ex = c()
for (i in (0:1000)){
  ex = c(ex,exp((-10)* i))
}
hist(Xn,freq=FALSE,breaks = c(0,0.005,0.020,0.030,0.040,0.05,0.06,0.08,0.1,0.12,0.15,0.2,0.25,0.3,0.4,0.5,0.7,1.322325),xlim=c(0,1.5),ylim=c(0,12),xlab="Tiempos entre llegadas",prob=T,ylab="Densidad de mensajes",main="Histograma de tiempo entre llegadas")
lines(density(Xn))
ggplot(data = midf)+
  geom_point(mapping = aes(x = Tiempo, y = trayectoriaMuestral)) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'))

ej = c(1,1,2,3,1,4,3,2,1)



hist(ej,breaks = c(1,3,4))

