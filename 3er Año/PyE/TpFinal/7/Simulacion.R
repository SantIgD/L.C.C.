library(ggplot2)
lambda = 10
intervaloTiempo=1000
Tiempo = c(1:intervaloTiempo)

Xn = c()
tActual = 0
trayectoria = c()
while (tActual <= intervaloTiempo){
  tiempoLLegada =rexp(1,lambda)/lambda
  trayectoria = c(trayectoria,tiempoLLegada)
  tActual = tActual + tiempoLLegada
  Xn = c(Xn,tiempoLLegada )
}

#c(0,0.01,0.02,0.03,0.04,0.05,0.65,0.07,0.08,0.1,0.12,0.15,0.2,0.3,0.4,0.948246)
# c(0,0.3,5,39.1022)
#par(mfrow=c(1,2))
hist(Xn,freq=FALSE,breaks = 50,xlim=c(0,0.1),ylim=c(0,100),xlab="Tiempos entre llegadas",prob=T,ylab="Densidad de mensajes",main="Histograma")
lines(density(Xn),col="red")
lines(curve(exp(-10*x),ylab = "Y",xlab = "X",main="Exp(-10*t)"))

midf= data.frame(trayectoria,Xn)
ggplot(data = midf)+
  geom_point(mapping = aes(x = Xn, y = trayectoria)) +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'))




axis(1, labels=TRUE)



