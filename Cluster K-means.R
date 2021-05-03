library(dplyr)
library(readxl)
library(tidyr)
library(animation)
library(factoextra)
library(NbClust)

group=read_excel("D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/Base de Datos FINAL/Base Datos R Actualizada 25-03-2021.xlsx")

pasture=group[,c(25,28,31)]
pasture=na.omit(pasture)


for(i in 1:3){
  pasture[,i] <- scale(pasture[,i], center = FALSE, scale = max(pasture[,i], na.rm = TRUE)/1)
}

wss <- 0

for (i in 1:15) {
  km.out <- kmeans(pasture, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

k=2
km.pasture <- kmeans(pasture, centers = k, nstart = 20, iter.max = 50)
write.table(km.pasture$cluster,"D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/Cluster Cropland.txt")
