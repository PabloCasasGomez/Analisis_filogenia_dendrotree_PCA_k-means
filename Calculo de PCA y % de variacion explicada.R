library(readxl)

#Cargamos y pretratamos los datos
genclim=read_excel("D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/Base de Datos FINAL/Base Datos R Actualizada 15-04-2021.xlsx")


#genclim=genclim[,c(6,7,13,14,23,24,25)]
#genclim$`Heterocigocidad E`=as.numeric(as.character(genclim$`Heterocigocidad E`))

genclim=genclim[,c(25,39:57)]
genclim=na.omit(genclim)

##Vemos el valor de la media para cada una de las variables
apply(X = genclim, MARGIN = 2, FUN = mean)

##Vemos el valor de la varianza para cada variable
apply(X = genclim, MARGIN = 2, FUN = var)

#Estandarizamos las variables para que sea comparable
genclimestand <- prcomp(genclim, scale = TRUE)
names(genclimestand)

genclimestand$center
genclimestand$scale

#Nos muestra para cada PC la combinacion lineal de cada variable, es el valor de cada fila * x para cada PC
genclimestand$rotation

#Cabeza de las primeras 5 muestras con cada PC
head(genclimestand$x)
dim(genclimestand$x)

#Representación grafica de las dos primeras componentes
biplot(x = genclimestand, scale = 0, cex = 0.6, col = c("blue4", "brown3"))


#Conocemos las PCA principales y ahora calculamos la varianza explicada por cada una de ellas
library(ggplot2)
genclimestand$sdev^2

prop_varianza <- genclimestand$sdev^2 / sum(genclimestand$sdev^2)
prop_varianza

#Representacion de cada % explicado
ggplot(data = data.frame(prop_varianza, pc = 1:20),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

#Proporcion de la varianza explicada acumulada
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

ggplot(data = data.frame(prop_varianza_acum, pc = 1:20),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

