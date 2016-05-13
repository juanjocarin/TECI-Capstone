rm(list=ls())
ruta<-"C:/Users/JuanJose/Google Drive/ANL/MASTERS/TECI/TFM/Universidad"
# Cambiar ruta segun proceda
setwd(ruta)


######## CARGA DATOS ########

MUS<-read.csv(file="all.csv")
# Tabla con las 57 ciudades con todas las variables
   # estandarizadas (media y desv. de la muestra)
rownames(MUS)<-MUS[,1]
ciudades<-rownames(MUS)
colnames(MUS)
MUS<-MUS[,-1]
summary(MUS)
# for (k in seq(1,71,10)){
#    if (k<71) kfin<-k+9 else kfin<-k+7
#    boxdata <- with(MUS,boxplot(MUS[,c(k:kfin)]),range=4)
#    for(i in 1:length(boxdata$group)){
#       text(boxdata$group[i],boxdata$out[i],
#            ciudades[if(
#               which(MUS[,c(k:kfin)]==
#                        boxdata$out[i])%%dim(MUS)[1]==0)
#               which(MUS[,c(k:kfin)]==boxdata$out[i])
#               else
#                  which(MUS[,c(k:kfin)]==
#                           boxdata$out[i])%%dim(MUS)[1]],
#            pos=4,cex=.75)
#    }
# }
Corr_MUS<-cor(MUS)["MUS.TRAFFIC",]
Corr_MUS[Corr_MUS>0.8]
#MUS<-MUS[,names(Corr_MUS[Corr_MUS>0.8])]

######## CLUSTERS JERARQUICOS ########
 
library(cluster)
distance<-dist(MUS)

# A la vista de la representacion anterior y
   # considerando los objetivos, se descarta los
   # metodos de:
      # WARD: muy sensible a Outliers y no buscamos
         # Clusters con similar nº de individuos
      # COMPLETO: tampoco interesa que los Clusters
         #tengan diametros similares
# Y en su lugar se consideraran:
      # MEDIO: Clusters con Varianzas similares
      # SIMPLE: Clusters irregulares. Busca el vecino
         # mas proximo.
      # CENTROIDE: la distancia entre Clusters es el
         # cuadrado de la distancia euclidea entre sus
         # centroides

fit_medio<-hclust(distance,method="average")
plot(fit_medio,hang=-1,main="Average Linkage Method")

# Una limitacion del Clustering Jerarquico es que
   # clasifica observaciones pero no explica en que
   # difiere cada clase/Cluster

# ¿Que numero de Clusters es el adecuado?
   # Podemos definir como metrica una usada en 
      # Particiones: la Anchura de la SILUETA de los
      # Clusters.
      # Para cada observacion se compara su cercania al
      # resto de miembros de su mismo Cluster con su
      # cercania a miembros de otros Clusters.
   # Valores proximos a 1 indican que la observacion
      # esta bien asignada a su Cluster.
   # Valores proximos a 0 indican que deberia asignarse
      # a otro Cluster.
   # Usaremos en primer lugar la Anchura Media de las
      # Siluetas: si es superior a 0.7 indicara una
      # fuerte estructura; si es inferior a 0.5, que la
      # estructura es debil o inexistente.

silueta<-numeric(28)
silueta[1]<-1
for (i in 2:28){
   silueta[i]<-summary(silhouette(cutree(fit_medio,k=i),
                                  distance))$avg.width
   }  
plot(silueta,type="b")
# La Anchura Media de la silueta baja de 0.7 a partir
   # de 3 Clusters.
# Pero además, a partir de 4 Clusters no hay buen
   # ajuste de algunas observaciones.
i<-4
plot(silhouette(cutree(fit_medio,k=i),distance))
# Parece razonable quedarse con 3 Clusters, incluyendo
   # el 2º y el 3º a los Outliers Madrid y Barcelona
   # (por separado)
         # addmargins(table(rownames(MUS),cutree(fit_medio,k=3)))
         # cutree(fit_medio, k=3)
plot(fit_medio,hang=-1,main="Average Linkage Method")
rect.hclust(fit_medio,k=3, border=2:4)

fit_simple<-hclust(distance,method="single")
plot(fit_simple,hang=-1,main="Single Linkage Method")
silueta<-numeric(28)
silueta[1]<-1
for (i in 2:28){
   silueta[i]<-summary(silhouette(cutree(fit_simple,k=i),
                                  distance))$avg.width
}  
plot(silueta,type="b")
# DE NUEVO La Anchura Media de la silueta baja de 0.7
   # a partir de 3 Clusters.
# Pero además, a partir de 4 Clusters no hay buen
   # ajuste de algunas observaciones.
i<-3
plot(silhouette(cutree(fit_simple,k=i),distance))
# Parece razonable quedarse con 3 Clusters, incluyendo
   # el 2º y el 3º a los Outliers Madrid y Barcelona
   # (por separado)
plot(fit_simple,hang=-1,main="Single Linkage Method")
rect.hclust(fit_simple,k=3, border=2:4)

fit_centroide<-hclust(distance,method="centroid")
plot(fit_centroide,hang=-1,main="Centroid Linkage Method")
silueta<-numeric(28)
silueta[1]<-1
for (i in 2:28){
   silueta[i]<-summary(silhouette(cutree(fit_centroide,
                                         k=i),
                                  distance))$avg.width
}
plot(silueta,type="b")
# DE NUEVO La Anchura Media de la silueta baja de 0.7
   # a partir de 3 Clusters.
# Pero además, a partir de 5 Clusters no hay buen
   # ajuste de algunas observaciones.
i<-5
plot(silhouette(cutree(fit_centroide,k=i),distance))
# Parece razonable quedarse con 3 Clusters, incluyendo
   # el 2º y el 3º a los Outliers Madrid y Barcelona
   # (por separado)
plot(fit_centroide,hang=-1,
     main="Centroid Linkage Method")
rect.hclust(fit_centroide,k=3, border=2:4)


######## CLUSTERS NO JERARQUICOS ########
######## K-MEDIAS ########

# Para determinar el nº optimo de Clusters analizamos
   # la suma de cuadrados de las distancias de las
   # observaciones en un mismo Cluster, para todos los
   # Clusters: tendremos asi una medida de lo compacto
   # de cada Cluster
# Probamos hasta 28 Clusters ya que interesan Clusters
   # con al menos 1 par de observaciones
errores<-numeric(28)
for(i in 1:28){
   set.seed(2096730329)
   errores[i]<-kmeans(MUS,centers=i)$tot.withinss}
plot(errores,type="b")
# El punto de inflexion parece estar en 5 Clusters, a
   # partir de ahí la agrupación se hace más compleja
   # sin un alto impacto en la minimizacion del error
kmeans(MUS,centers=3)$cluster

# Tambien podemos comprobar las distancias entre
   # Clusters
dist_clusters<-numeric(28)
for(i in 1:28){
   set.seed(2096730329)
  dist_clusters[i]<-kmeans(MUS,centers=i)$betweenss
}
plot(dist_clusters,type="b")
# La grafica vuelve a sugerir 5 clusters

# O, mejor aun, calcular el indice CH (Calinski-
   # Harabasz), que nos permite calcular el numero k
   # de clusters que maximiza la relacion entre una
   # distancia total intra-cluster baja y una distancia
   # inter-cluster alta
CH_index<-function(datos,k){
   set.seed(2096730329)
   fit<-kmeans(datos,k)
   CH_index<-(fit$betweenss/(k-1))/
      (fit$tot.withinss/(dim(datos)[1]-k))
}
CH_k<-numeric(28)
CH_k<-sapply(1:28,function(i) CH_index(MUS,i))
which(CH_k==max(CH_k))
plot(c(1:28),CH_k,type="b",xlab="k",ylab="CH(k)",xlim=c(2,28), xaxt='n')
axis(1,at=seq(0, 28, 2))
# En puridad el maximo se da con 2 clusters, pero vamos
   # a quedarnos con el sugiente pico, en k=5 o 6
      # El indice CH funciona bien con clusters del
         # mismo tamaño, y aqui no es el caso

# Nos quedamos por tanto con 5 Clusters
set.seed(2096730329)
fit_kmedias<-kmeans(MUS,4)
summary(fit_kmedias)
str(fit_kmedias)
fit_kmedias$cluster
fit_kmedias$withins
addmargins(table(rownames(MUS),fit_kmedias$cluster))
Clusters<-sapply(1:5,function(i){
   fit_kmedias$cluster[fit_kmedias$cluster==i]})
print(Clusters)
library(fpc)
plotcluster(MUS,fit_kmedias$cluster)
clusplot(MUS,fit_kmedias$cluster,color=F,shade=T,
         labels=2,cex=0.75)

ord <- cmdscale(distance,eig=T)
plot(ord$points)

ordihull(ord,fit_kmedias$cluster,lty=10)
ordispider(ord,fit_kmedias$cluster,col="blue",
           label=TRUE)

######## PAM ########

pamk.best<-pamk(data=MUS,krange=28,criterion="asw",
                  critout=T)
pam3<-pamk(data=MUS,krange=5,criterion="asw",
           critout=T)
pam(MUS,pam3$nc)
pamk.best$pamobject[[3]]
pam3$pamobject[[3]]
clusplot(pam(MUS,pam3$nc),lines=0,color=F,shade=F,
         labels=2,cex=0.75)


ord <- cmdscale(distance,eig=T)
plot(ord$points)

ordihull(ord,pam3$pamobject[[3]],lty=10)
ordispider(ord,pam3$pamobject[[3]],col="blue",
           label=TRUE)

######### The optimal model according to BIC for EM
   # initialized by hierarchical clustering for
   # parameterized Gaussian mixture models####

library(mclust)
d_clust<-Mclust(as.matrix(MUS),G=1:28)
m.best<-dim(d_clust$z)[2]
cat("model-based optimal number of clusters:",m.best,
    "\n")
windows()
plot(d_clust)
d_clust$classification

ord <- cmdscale(distance,eig=T)
plot(ord$points)
ordihull(ord,d_clust$classification,lty=10)
ordispider(ord,d_clust$classification,col="blue",
           label=TRUE)

######### AFFINITY PROPAGATION CLUSTERING ####
   # Affinity Propagation clusters data using a set of
      # real-valued pairwise data point similarities as
      # input. Each cluster is represented by a cluster
      # center data point (the so-called exemplar). The
      # method is iterative and searches for clusters
      # maximizing an objective function called net
      # similarity.

library(apcluster)
fit_AP<-apcluster(negDistMat(r=2),x=MUS,details=T)
cat("affinity propogation optimal number of clusters:",
    length(fit_AP@clusters), "\n")
# 13
fit_AP
fit_AP@sim
windows()
apcluster::heatmap(fit_AP)
plot(fit_AP,MUS)