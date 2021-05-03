library(ape)
library(treeio)
library(readxl)
library(phylobase)
library(phytools)

genetica=read_excel("D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/Base de Datos FINAL/Base Datos R Actualizada 15-04-2021.xlsx")

Gen=read.nexus("D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/TREEBASE/T107816.nex")

total=Gen$tip.label
total=tolower(total)

mis_especies=(unique(genetica$`Especie R Filogenia`))
mis_especies=gsub(' ','_',mis_especies)
mis_especies=tolower(mis_especies)

borrar=setdiff(total,mis_especies)

interseccion=intersect(mis_especies,total)
bi_subset <- tree_subset(Gen,interseccion, levels_back = 1)


Gen$tip.label=tolower(Gen$tip.label)
final=drop.tip(Gen,borrar)
  
plot(final)
write.tree(final,"D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/TREEBASE/resultado_with_edge_length.txt")

final$edge.length=NULL
plot(final)
write.tree(final,"D:/pablo/Desktop/Lo mas actualizado/Doctorado/6º Manuscrito (Variablididad Genética)/TREEBASE/resultado_without_edge_length.txt")

#bi_subset <- tree_subset(Gen,"Abies_nebrodensis" , levels_back = 4)
#plot(bi_subset)
