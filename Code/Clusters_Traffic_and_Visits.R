rm(list=ls())
ruta<-"C:/Users/JuanJose/Google Drive/ANL/MASTERS/TECI/TFM/Universidad"
# Cambiar ruta segun proceda
setwd(ruta)


######## CARGA DATOS ########

MUS<-read.csv(file="traffic_and_visits.csv")
# Tabla con las 57 ciudades con la media semanal de
   # Trafico y Visitas de MUS estandarizadas (media y
   # desv. de la muestra)
rownames(MUS)<-MUS[,1]
ciudades<-rownames(MUS)
colnames(MUS)
MUS<-MUS[,-1]
colnames(MUS)<-c("Disp_Visits","Traffic")
cor(MUS)
# Alta correlacion entre ambas variables
summary(MUS)
boxplot(x=MUS)
   # El Trafico presenta mayor dispersion que las Visitas
# Las Visitas presentan mas Outliers (y mas alejados)

# Para detectar los Outliers
boxdata <- with(MUS,boxplot(MUS),range=4)
for(i in 1:length(boxdata$group)){
   text(boxdata$group[i],boxdata$out[i],
        ciudades[if(which(MUS==boxdata$out[i])==
                       dim(MUS)[1])dim(MUS)[1]
                 else
                    which(MUS==
                             boxdata$out[i])%%dim(MUS)[1]],
        pos=4)
}
# Los principales Outliers para ambas variables son
   # Madrid y Barcelona, sobre todo en Visitas
# Visitas presenta menos Variabilidad, pero aparecen 4
   # Outliers mas: Valencia, Sevilla, Malaga y Zaragoza
   # (las otras ciudades mas pobladas, casi por orden
   # (se invierten Zaragoza y Malaga))

plot(MUS,xlab="Display Visits",ylab="Traffic")
text(x=MUS$Disp_Visits,y=MUS$Traffic,
     labels=rownames(MUS),cex=0.6,col="red")
# Se aprecia de nuevo que Madrid y Barcelona son
   # Outliers (seguidos de Valencia y Sevilla)

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
# Pero además, a partir de 5 Clusters no hay buen
   # ajuste de algunas observaciones.
i<-5
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
i<-4
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
# El punto de inflexion parece estar en 3 Clusters, a
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

# Nos quedamos por tanto con 3 Clusters
set.seed(2096730329)
fit_kmedias<-kmeans(MUS,3)
summary(fit_kmedias)
str(fit_kmedias)
fit_kmedias$cluster
fit_kmedias$withins
addmargins(table(rownames(MUS),fit_kmedias$cluster))
Clusters<-sapply(1:3,function(i){
   fit_kmedias$cluster[fit_kmedias$cluster==i]})
print(Clusters)
library(fpc)
#plotcluster(MUS,fit_kmedias$cluster)
clusplot(MUS,fit_kmedias$cluster,color=F,shade=T,
         labels=2,cex=0.75)

######### 30 indices for determining the number of
# clusters and proposes to user the best clustering
# scheme from the different results obtained by
# varying all combinations of number of clusters,
# distance measures, and clustering methods

library(NbClust)
(nb <- NbClust(MUS, diss="NULL", distance = "euclidean", 
               min.nc=2, max.nc=28, method = "kmeans", 
               index = "alllong", alphaBeale = 0.1))
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

######## PAM ########

pamk.best<-pamk(data=MUS,krange=28,criterion="asw",
                  critout=T)
plot(pam(MUS,pamk.best$nc))
plot(pam(MUS,2))
pam3<-pamk(data=MUS,krange=3,criterion="asw",
           critout=T)
pam(MUS,pam3$nc)
pamk.best$pamobject[[3]]
pam3$pamobject[[3]][pam3$pamobject[[3]]==1]
clusplot(pam(MUS,pam3$nc),lines=0,color=F,shade=F,
         labels=2,cex=0.75)

######### The optimal model according to BIC for EM
   # initialized by hierarchical clustering for
   # parameterized Gaussian mixture models####

library(mclust)
d_clust<-Mclust(as.matrix(MUS),G=1:28)
m.best<-dim(d_clust$z)[2]
cat("model-based optimal number of clusters:",m.best,
    "\n")
plot(d_clust)
d_clust$classification

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
# 8
fit_AP
fit_AP@sim
windows()
apcluster::heatmap(fit_AP)
plot(fit_AP,MUS)

## Cambiar la preferencia al cuantil 10% de
   # similaridades
fit_AP2<-apcluster(s=fit_AP@sim,q=0.1)
show(fit_AP2)
plot(fit_AP2,MUS)

## now try the same with RBF kernel
sim <- expSimMat(MUS, r=2)
apres <- apcluster(s=sim, q=0.1)
show(apres)
plot(apres,MUS)