library(ggplot2)
In <- sample(0:1,10,replace = TRUE) # 100 tiradas de una moneda
Tiradas <- c(1:10)


Dn <- c()

for (i in In){
  Dn <- c(Dn,(2 * i) - 1)
}
fra_p2 <-cumsum(Dn)
dibujar <- data.frame(In,Dn)
proceso1 <- data.frame(In,Tiradas)
proceso2 <- data.frame(Dn,Tiradas)


mi_df<- data.frame(
  "Proceso1" = In,
  "Proceso2" = fra_p2
)

ggplot(data = mi_df) +
  geom_point(mapping = aes(x = proceso1, y = proceso2))

ggplot(mi_df, aes(Tiradas)) +                    # basic graphical object
  geom_line(aes(y=In), colour="darkgreen") +  # first layer
  geom_line(aes(y=fra_p2), colour="blue") + # second layer
  geom_line(aes(y=Dn), colour="brown") + # second layer
  ggtitle("Tiradas de moneda vs Cambio de posicion vs Desplazamiento")+
theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = 'grey', colour = 'pink'))+
  ylab("In/ Dn / Sn")  + scale_fill_manual(breaks = c(1:10))
  


ejercicio2 <- data.frame(
  "In" = In,
  "Dn" = Dn,
  "Sn" = fra_p2
)
write.table(ejercicio2)
write.table(x = ejercicio2, file = ".txt", sep = ",", 
            row.names = FALSE, col.names = TRUE)
