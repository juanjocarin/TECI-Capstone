######## INICIALIZACION ########

rm(list=ls())
ruta<-"C:/Users/JuanJose/Google Drive/ANL/MASTERS/TECI/TFM/Universidad"
# Cambiar ruta segun proceda
setwd(ruta)

######## CARGA LIBRERIAS ########

library(cluster)
library(dtw)
library(TSclust)
library(fpc)
library(ade4)


######## CARGA FUNCIONES ########

posicion<-function(distancia,coord,N){
   if (coord%%N==0){
      fila<-dim(distancia)[1];
      columna<-coord/N} else {
         fila<-coord%%N;
         columna<-floor(coord/N)+1}
   lista<-list(fila,columna)
}

empareja<-function(vecinos,N){
   parejas<-data.frame()
   j<-0
   for (i in 1:N){
   if(vecinos[rownames(vecinos)==vecinos[i,],]==
         rownames(vecinos)[i]){
      if(j==0){
         j<-j+1
         parejas[j,1]<-rownames(vecinos)[i]
         parejas[j,2]<-vecinos[rownames(vecinos)[i],]
      }else if (sum(sapply(rownames(vecinos)[i]==
                              parejas[c(1:j),2],
                           prod))!=1){
         j<-j+1
         parejas[j,1]<-rownames(vecinos)[i]
         parejas[j,2]<-vecinos[rownames(vecinos)[i],]}
   }
   }
   colnames(parejas)<-c("Ciudad","Vecina")
   parejas[,2]<-as.character(parejas[,2])
   parejas
}


num2bin<-function(number,noBits){
   binary_vector=rev(as.numeric(intToBits(number)))
   if(missing(noBits))
      binary_vector
   else
      binary_vector[-(1:(length(binary_vector)-noBits))]
}

parejas1<-p1
parejas2<-p2
optimiza<-function(parejas1,parejas2,num,numBits,maximo){
   distTC<-Inf
   Test_loop<-matrix(NA,dim(parejas2)[1]+1,1)
   Ctrl_loop<-Test_loop
   for (i in inicio:num){
      Test_loop[1]<-as.matrix(parejas1[,1])
      Ctrl_loop[1]<-as.matrix(parejas1[,2])
      d_aux<-Inf
      for (j in 1:numBits){
         if(num2bin(i,numBits)[j]==0){
            Test_loop[j+1]<-parejas2[j,1];
            Ctrl_loop[j+1]<-parejas2[j,2]
         }else{
            Test_loop[j+1]<-parejas2[j,2];
            Ctrl_loop[j+1]<-parejas2[j,1]}
      }
      GMAs_Test<-matrix(NA,1,dim(parejas2)[1]+1)
      names(GMAs_Test)<-Test_loop
      GMAs_Ctrl<-matrix(NA,1,dim(parejas2)[1]+1)
      names(GMAs_Ctrl)<-Ctrl_loop
      for (k in Test_loop){
         GMAs_Test[k]<-GMAs[GMAs[,1]==k,2]   
      }
      for (k in Ctrl_loop){
         GMAs_Ctrl[k]<-GMAs[GMAs[,1]==k,2]   
      }
      if (table(GMAs_Test %in% GMAs_Ctrl)["FALSE"]==
             dim(Test_loop)[1]){
         GTest<-TSTrafico[,Test_loop]
         GCtrl<-TSTrafico[,Ctrl_loop]
         d_aux<-diss(as.data.frame(cbind(rowSums(GTest),
                                         rowSums(GCtrl))),
                     "DWT",diag=T)/maximo
         distTC<-min(distTC,d_aux)
         if(d_aux==distTC){
            Test<-Test_loop;
            Ctrl<-Ctrl_loop}}
   }
   lista<-list(distTC,d,Test,Ctrl)
}


representa<-function(GTestTrafico,GCtrlTrafico,
                     GTestTrafico_rnd,GCtrlTrafico_rnd,
                     GTestTrafico_rnd_TOTAL,
                     GCtrlTrafico_rnd_TOTAL,
                     GTestVisitas,GCtrlVisitas,
                     GTestVisitas_rnd,GCtrlVisitas_rnd,
                     GTestVisitas_rnd_TOTAL,
                     GCtrlVisitas_rnd_TOTAL){
   ymax<-colMaxs(rowMaxs(cbind(
      rowSums(GTestTrafico),
      rowSums(GCtrlTrafico),
      rowSums(GTestTrafico_rnd),
      rowSums(GCtrlTrafico_rnd),
      rowSums(GTestTrafico_rnd_TOTAL),
      rowSums(GCtrlTrafico_rnd_TOTAL))))
   ymax2<-colMaxs(rowMaxs(cbind(
      rowSums(GTestVisitas),
      rowSums(GCtrlVisitas),
      rowSums(GTestVisitas_rnd),
      rowSums(GCtrlVisitas_rnd),
      rowSums(GTestVisitas_rnd_TOTAL),
      rowSums(GCtrlVisitas_rnd_TOTAL))))
   
   texto1<-"Optimal assignment of the"
   texto2<-"Random assignment of the "
   texto_fin<-"best maching GMAs"
   texto3<-"Random assignment of 56 of the 57 GMAs"
   numero<-as.character(2*ncol(GTestTrafico))
   
   plot(rowSums(GTestTrafico),type="l",col="blue",
        ylim=c(0,ymax),lwd=2,
        main=paste("Traffic of Groups 1 and 2",
                   "\n",texto1,numero,texto_fin),
        xlab="Pre-test period Week",ylab="Traffic")
   lines(rowSums(GCtrlTrafico),col="red",lwd=2)
   legend("topleft",legend = c("Group 1","Group 2"),lty=1,
          col=c("blue","red"),bty="n",lwd=2)
   
   plot(rowSums(GTestTrafico_rnd),type="l",col="cyan",
        ylim=c(0,ymax),lwd=2,
        main=paste("Traffic of Groups 1 and 2",
                   "\n",texto2,numero,texto_fin),
        xlab="Pre-test period Week",ylab="Traffic")
   lines(rowSums(GCtrlTrafico_rnd),col="orange",lwd=2)
   legend("topleft",legend = c("Group 1","Group 2"),lty=1,
          col=c("cyan","orange"),bty="n",lwd=2)
   
   plot(rowSums(GTestTrafico_rnd_TOTAL),type="l",
        col="green",ylim=c(0,ymax),lwd=2,
        main=paste("Traffic of Groups 1 and 2",
                   "\n",texto3),
        xlab="Pre-test period Week",
        ylab="Traffic")
   lines(rowSums(GCtrlTrafico_rnd_TOTAL),
         col="darkorange",lwd=2)
   legend("topleft",legend = c("Group 1","Group 2"),lty=1,
          col=c("green","darkorange"),bty="n",lwd=2)
   
   plot(rowSums(GTestVisitas),type="l",col="blue",
        ylim=c(0,ymax2),lwd=2,
        main=paste("Display Visits of Groups 1 and 2",
                   "\n",texto1,numero,texto_fin),
        xlab="Pre-test period Week",
        ylab="Display Visits")
   lines(rowSums(GCtrlVisitas),col="red",lwd=2)
   legend("topright",legend = c("Group 1","Group 2"),lty=1,
          col=c("blue","red"),bty="n",lwd=2)
   plot(rowSums(GTestVisitas_rnd),type="l",col="cyan",
        ylim=c(0,ymax2),lwd=2,
        main=paste("Display Visits of Groups 1 and 2",
                   "\n",texto2,numero,texto_fin),
        xlab="Pre-test period Week",
        ylab="Display Visits")
   lines(rowSums(GCtrlVisitas_rnd),col="orange",lwd=2)
   legend("topright",legend = c("Group 1","Group 2"),lty=1,
          col=c("cyan","orange"),bty="n",lwd=2)
   plot(rowSums(GTestVisitas_rnd_TOTAL),type="l",
        col="green",ylim=c(0,ymax2),lwd=2,
        main=paste("Display Visits of Groups 1 and 2",
                   "\n",texto3),
        xlab="Pre-test period Week",
        ylab="Display Visits")
   lines(rowSums(GCtrlVisitas_rnd_TOTAL),col="darkorange",
         lwd=2)
   legend("topright",legend = c("Group 1","Group 2"),lty=1,
          col=c("green","darkorange"),bty="n")
}


######## CARGA DATOS ########

TSTrafico<-read.csv(file="TimeSeriesTrafico.csv")
NCiudades<-ncol(TSTrafico)
Nsemanas<-nrow(TSTrafico)
TSVisitas<-read.csv(file="TimeSeriesVisitasDisp.csv")

GMAs<-read.csv("GMAs.csv")
GMAs[,1]<-as.character(GMAs[,1])

plot(c(1:Nsemanas),rowSums(TSTrafico),type="l",
     xlab="Semana",ylab="Trafico Conc. y Visitas Web DISP (normalizados)",
     main="Trafico Conc. y Visitas Web DISP en las 57 ciudades",
     col="blue",ylim=c(0,4),lwd=2)
lines(c(1:Nsemanas),rowSums(TSVisitas),col="red",lwd=2)
legend("topright",legend = c("Trafico","Visitas DISP"),lty=1,
       col=c("blue","red"),cex=.75,lwd=2)

sum(rowSums(TSTrafico))
sum(rowSums(TSVisitas))
# La suma de la fila iesima da el % de la Var.
   # en l semana i, para todas las Ciudades
# La suma de la columna iesima da el % de la Var.
   # en la Ciudad i, para todo el periodo de Estudio 


######## DISTANCIAS: DWT (Wavelet) ########

distTrafico<-as.data.frame(
   as.matrix(diss(TSTrafico,"DWT",diag=T)))
distVisitas<-as.data.frame(
   as.matrix(diss(TSVisitas,"DWT",diag=T)))

var(rowSums(TSTrafico))
# 0.04
var(rowSums(TSVisitas))
# 0.75
sum_distTrafico<-sum(rowSums(distTrafico))/2
# 239.585
sum_distVisitas<-sum(rowSums(distVisitas))/2
# 506.842
max_distTrafico<-max(distTrafico[,1:NCiudades])
# 0.791
max_distVisitas<-max(distVisitas[,1:NCiudades])
# 3.580
# Para tener una mejor noción de lo que suponen las
   # distancias, normalizaremos respecto a la maxima
   # distancia entre 2 Ciudades
coord_max_distTrafico<-which(distTrafico==
                               max(distTrafico))[1]
lista<-posicion(distTrafico,coord_max_distTrafico,
                NCiudades)
row<-lista[[1]];col<-lista[[2]]
rownames(distTrafico)[row]
colnames(distTrafico)[col]
coord_max_distVisitas<-which(distVisitas==
                                max(distVisitas))[1]
lista<-posicion(distVisitas,coord_max_distVisitas,
                NCiudades)
row<-lista[[1]];col<-lista[[2]]
rownames(distVisitas)[row]
colnames(distVisitas)[col]
# MADRID es la Ciudad más alejada de cualquier otra
   # en ambos casos: de BEJAR en Trafico y de LEIOA en
   # Visitas
distTrafico<-distTrafico/max_distTrafico
distVisitas<-distVisitas/max_distVisitas
sum_distTrafico<-sum(rowSums(distTrafico))/2
sum_distVisitas<-sum(rowSums(distVisitas))/2
# Si dividimos por maxTrafico (o maxVisitas) las
   # Series Temporales, se obtiene tambien una
   # distancia maxima = 1 entre la pareja de Ciudades
   # citada


######## VECINOS + PROXIMOS Y PAREJAS ########

Vecinos<-sapply(1:NCiudades,function(i){
   rownames(distTrafico)[
      which(distTrafico[i,]==min(
         distTrafico[i,distTrafico[i,]!=0]))]})
Vecinos<-as.data.frame(Vecinos)
rownames(Vecinos)<-rownames(distTrafico)

k<-3
knumVecinos<-matrix(0,ncol=k,nrow=NCiudades)
kVecinos<-knumVecinos
for (i in 1:NCiudades){
   knumVecinos[i,]<-order(distTrafico[i,])[2:(k+1)]
   for (j in 1:k)
      kVecinos[i,j]<-colnames(distTrafico[1,])[
         knumVecinos[i,j]]
}
rownames(kVecinos)<-colnames(distTrafico)
kVecinos<-as.data.frame(kVecinos)
                        
parejas<-empareja(Vecinos,NCiudades)

GMAs_Test<-NULL
GMAs_Ctrl<-NULL
for (k in parejas[,1]){
   GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
}
for (k in parejas[,2]){
   GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
}
mismoGMA<-GMAs_Test==GMAs_Ctrl
which(mismoGMA==T)
parejas<-parejas[which(mismoGMA==F),]   

p1<-parejas[1,]
p2<-parejas[c(2:dim(parejas)[1]),]
num<-2^(dim(p2)[1])-1
numBits<-dim(p2)[1]

lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico<-lista[[1]]
d<-lista[[2]]
Test<-lista[[3]]
Ctrl<-lista[[4]]
GTestTrafico<-TSTrafico[,Test]
GCtrlTrafico<-TSTrafico[,Ctrl]
GTestVisitas<-TSVisitas[,Test]
GCtrlVisitas<-TSVisitas[,Ctrl]
(cor(rowSums(GTestTrafico),rowSums(GCtrlTrafico)))
(cor(rowSums(GTestVisitas),rowSums(GCtrlVisitas)))
distTCVisitas<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas),rowSums(GCtrlVisitas))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas<-c(as.character(Test),as.character(Ctrl))
GTotalTrafico_rnd<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd<-GTotalTrafico_rnd
for (i in 1:length(array_parejas)){
   GTotalTrafico_rnd[,i]<-
      TSTrafico[,array_parejas[i]]
   GTotalVisitas_rnd[,i]<-
      TSVisitas[,array_parejas[i]]
   colnames(GTotalTrafico_rnd)[i]<-
      array_parejas[i]
   colnames(GTotalVisitas_rnd)[i]<-
      array_parejas[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas),
           length(array_parejas)/2,replace=FALSE) 
v2<-c(1:length(array_parejas))[-v1]
GTestTrafico_rnd<-GTotalTrafico_rnd[v1]
GCtrlTrafico_rnd<-GTotalTrafico_rnd[v2]
GTestVisitas_rnd<-GTotalVisitas_rnd[v1]
GCtrlVisitas_rnd<-GTotalVisitas_rnd[v2]
distTCTrafico_rnd<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd),
         rowSums(GCtrlTrafico_rnd))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd),
    rowSums(GCtrlTrafico_rnd))
distTCTrafico_rnd
distTCTrafico
distTCVisitas_rnd<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd),
         rowSums(GCtrlVisitas_rnd))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd),
    rowSums(GCtrlVisitas_rnd))
distTCVisitas_rnd
distTCVisitas

set.seed(123456789)
v1<-sample(NCiudades,NCiudades/2,replace=F) 
v2<-c(1:NCiudades)[-v1]
length(sort(c(v1,v2)))
if(NCiudades%%2!=0)
   v2<-v2[randconf(NCiudades/2+1,NCiudades/2)]
GTestTrafico_rnd_TOTAL<-TSTrafico[v1]
GCtrlTrafico_rnd_TOTAL<-TSTrafico[v2]
GTestVisitas_rnd_TOTAL<-TSVisitas[v1]
GCtrlVisitas_rnd_TOTAL<-TSVisitas[v2]
dTCTrafico_rnd_TOTAL<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd_TOTAL),
         rowSums(GCtrlTrafico_rnd_TOTAL))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd_TOTAL),
    rowSums(GCtrlTrafico_rnd_TOTAL))
dTCVisitas_rnd_TOTAL<-
   diss(as.data.frame(
      cbind(rowSums(GTestVisitas_rnd_TOTAL),
            rowSums(GCtrlVisitas_rnd_TOTAL))),"DWT",
      diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd_TOTAL),
    rowSums(GCtrlVisitas_rnd_TOTAL))


######## REPRESENTACION ########

representa(GTestTrafico,GCtrlTrafico,
           GTestTrafico_rnd,GCtrlTrafico_rnd,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas,GCtrlVisitas,
           GTestVisitas_rnd,GCtrlVisitas_rnd,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)

mds_coord<-cmdscale(distTrafico,eig=T,x.ret=T,add=F)
mds_coord$GOF
max(mds_coord$points[,1])
min(mds_coord$points[,1])
max(mds_coord$points[,2])
min(mds_coord$points[,2])
plot(NULL,xlim=c(-.3,0.9),ylim=c(-0.1,0.1),asp=1,axes=T,lty=2)
s.label(mds_coord$points,xax=1,yax=2,
        label=row.names(distTrafico),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.85),ylim=c(-0.2,0.1),
        cpoint=1,add.plot=T)
s.label(mds_coord$points,xax=1,yax=2,
        label=row.names(distTrafico),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.3),ylim=c(-0.2,0.15),
        cpoint=1)


######## VECINOS Y PAREJAS AL REDUCIR LA DIM. ########

distTrafico_MDS<-as.data.frame(as.matrix(
   dist(mds_coord$points)))
# distTrafico_MDS["Madrid","Bejar"]
# Se preserva en gran medida la distancia unitaria
   # Madrid-Bejar: 0.9985133

Vecinos2<-sapply(1:NCiudades,function(i){
   rownames(distTrafico_MDS)[
      which(distTrafico_MDS[i,]==min(
         distTrafico_MDS[i,distTrafico_MDS[i,]!=0]))]})
Vecinos2<-as.data.frame(Vecinos2)
rownames(Vecinos2)<-rownames(distTrafico_MDS)

posibles_parejas<-empareja(Vecinos2,NCiudades)
parejas2<-parejas
for (i in 1:dim(posibles_parejas)[1]){
   if(
      length(which(parejas==
                      posibles_parejas[i,1]))==0 &
         length(which(parejas==
                         posibles_parejas[i,2]))==0)
         parejas2<-rbind(parejas2,posibles_parejas[i,])
}
rownames(parejas2)<-c(1:dim(parejas2)[1])
nuevas_parejas<-parejas2[-c(1:dim(parejas)[1]),]
parejas2<-rbind(parejas,nuevas_parejas)

   GMAs_Test<-NULL
   GMAs_Ctrl<-NULL
   for (k in parejas2[,1]){
      GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
   }
   for (k in parejas2[,2]){
      GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
   }
   mismoGMA<-GMAs_Test==GMAs_Ctrl
   which(mismoGMA==T)
   parejas2<-parejas2[which(mismoGMA==F),]   
   
   p1<-parejas2[1,]
   p2<-parejas2[c(2:dim(parejas2)[1]),]
   num<-2^(dim(p2)[1])-1
   numBits<-dim(p2)[1]

lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico2<-lista[[1]]
d2<-lista[[2]]
Test2<-lista[[3]]
Ctrl2<-lista[[4]]
GTestTrafico2<-TSTrafico[,Test2]
GCtrlTrafico2<-TSTrafico[,Ctrl2]
GTestVisitas2<-TSVisitas[,Test2]
GCtrlVisitas2<-TSVisitas[,Ctrl2]
(cor(rowSums(GTestTrafico2),rowSums(GCtrlTrafico2)))
(cor(rowSums(GTestVisitas2),rowSums(GCtrlVisitas2)))
distTCVisitas2<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas2),rowSums(GCtrlVisitas2))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas2<-c(as.character(Test2),as.character(Ctrl2))
GTotalTrafico_rnd2<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd2<-GTotalTrafico_rnd2
for (i in 1:length(array_parejas2)){
   GTotalTrafico_rnd2[,i]<-
      TSTrafico[,array_parejas2[i]]
   GTotalVisitas_rnd2[,i]<-
      TSVisitas[,array_parejas2[i]]
   colnames(GTotalTrafico_rnd2)[i]<-
      array_parejas2[i]
   colnames(GTotalVisitas_rnd2)[i]<-
      array_parejas2[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas2),
           length(array_parejas2)/2,replace=FALSE) 
v2<-c(1:length(array_parejas2))[-v1]
GTestTrafico_rnd2<-GTotalTrafico_rnd2[v1]
GCtrlTrafico_rnd2<-GTotalTrafico_rnd2[v2]
GTestVisitas_rnd2<-GTotalVisitas_rnd2[v1]
GCtrlVisitas_rnd2<-GTotalVisitas_rnd2[v2]
distTCTrafico_rnd2<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd2),
         rowSums(GCtrlTrafico_rnd2))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd2),
    rowSums(GCtrlTrafico_rnd2))
distTCTrafico_rnd2
distTCTrafico2
distTCVisitas_rnd2<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd2),
         rowSums(GCtrlVisitas_rnd2))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd2),
    rowSums(GCtrlVisitas_rnd2))
distTCVisitas_rnd2
distTCVisitas2


######## REPRESENTACION ########

representa(GTestTrafico2,GCtrlTrafico2,
           GTestTrafico_rnd2,GCtrlTrafico_rnd2,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas2,GCtrlVisitas2,
           GTestVisitas_rnd2,GCtrlVisitas_rnd2,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)


######## VECINOS + PROXIMOS Y PAREJAS en las ciudades restantes ########

ciudades<-sort(colnames(TSTrafico))
ciudades2<-sort(array_parejas2)
ciudades_Restantes<-ciudades
for (i in 1:length(array_parejas2)){
   ciudades_Restantes<-
      ciudades_Restantes[which(
         ciudades_Restantes!=ciudades2[i])]
}
NCiudades_Restantes<-length(ciudades_Restantes)

distTrafico_21<-distTrafico[ciudades_Restantes,
                            ciudades_Restantes]
sum_distTrafico_21<-rowSums(distTrafico_21)
names(sum_distTrafico_21[sum_distTrafico_21==
                            max(sum_distTrafico_21)])
# Gijon es la ciudad mas alejada del resto

Vecinos3<-sapply(1:NCiudades_Restantes,function(i){
   rownames(distTrafico_21)[
      which(distTrafico_21[i,]==min(
         distTrafico_21[i,distTrafico_21[i,]!=0]))]})
Vecinos3<-as.data.frame(Vecinos3)
rownames(Vecinos3)<-rownames(distTrafico_21)

nuevas_parejas2<-empareja(Vecinos3,NCiudades_Restantes)

   parejas3<-rbind(parejas2,nuevas_parejas2)
   
   GMAs_Test<-NULL
   GMAs_Ctrl<-NULL
   for (k in parejas3[,1]){
      GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
   }
   for (k in parejas3[,2]){
      GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
   }
   mismoGMA<-GMAs_Test==GMAs_Ctrl
   which(mismoGMA==T)
   parejas3<-parejas3[which(mismoGMA==F),]   
   
   p1<-parejas3[1,]
   p2<-parejas3[c(2:dim(parejas3)[1]),]
   num<-2^(dim(p2)[1])-1
   numBits<-dim(p2)[1]

lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico3<-lista[[1]]
d3<-lista[[2]]
Test3<-lista[[3]]
Ctrl3<-lista[[4]]
GTestTrafico3<-TSTrafico[,Test3]
GCtrlTrafico3<-TSTrafico[,Ctrl3]
GTestVisitas3<-TSVisitas[,Test3]
GCtrlVisitas3<-TSVisitas[,Ctrl3]
(cor(rowSums(GTestTrafico3),rowSums(GCtrlTrafico3)))
(cor(rowSums(GTestVisitas3),rowSums(GCtrlVisitas3)))
distTCVisitas3<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas3),rowSums(GCtrlVisitas3))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas3<-c(as.character(Test3),as.character(Ctrl3))
GTotalTrafico_rnd3<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd3<-GTotalTrafico_rnd3
for (i in 1:length(array_parejas3)){
   GTotalTrafico_rnd3[,i]<-
      TSTrafico[,array_parejas3[i]]
   GTotalVisitas_rnd3[,i]<-
      TSVisitas[,array_parejas3[i]]
   colnames(GTotalTrafico_rnd3)[i]<-
      array_parejas3[i]
   colnames(GTotalVisitas_rnd3)[i]<-
      array_parejas3[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas3),
           length(array_parejas3)/2,replace=FALSE) 
v2<-c(1:length(array_parejas3))[-v1]
GTestTrafico_rnd3<-GTotalTrafico_rnd3[v1]
GCtrlTrafico_rnd3<-GTotalTrafico_rnd3[v2]
GTestVisitas_rnd3<-GTotalVisitas_rnd3[v1]
GCtrlVisitas_rnd3<-GTotalVisitas_rnd3[v2]
distTCTrafico_rnd3<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd3),
         rowSums(GCtrlTrafico_rnd3))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd3),
    rowSums(GCtrlTrafico_rnd3))
distTCTrafico_rnd3
distTCTrafico3
distTCVisitas_rnd3<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd3),
         rowSums(GCtrlVisitas_rnd3))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd3),
    rowSums(GCtrlVisitas_rnd3))
distTCVisitas_rnd3
distTCVisitas3


######## REPRESENTACION ########

representa(GTestTrafico3,GCtrlTrafico3,
           GTestTrafico_rnd3,GCtrlTrafico_rnd3,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas3,GCtrlVisitas3,
           GTestVisitas_rnd3,GCtrlVisitas_rnd3,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)

mds_coord2<-cmdscale(distTrafico_21,eig=T,x.ret=T,add=F)
mds_coord2$GOF
max(mds_coord2$points[,1])
min(mds_coord2$points[,1])
max(mds_coord2$points[,2])
min(mds_coord2$points[,2])
plot(NULL,xlim=c(-.3,0.9),ylim=c(-0.2,0.1),asp=1,axes=T,lty=2)
s.label(mds_coord2$points,xax=1,yax=2,
        label=row.names(distTrafico_21),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.85),ylim=c(-0.2,0.1),cpoint=1,add.plot=T)
s.label(mds_coord2$points,xax=1,yax=2,
        label=row.names(distTrafico_21),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.3),ylim=c(-0.2,0.15),cpoint=1)


######## VECINOS Y PAREJAS AL REDUCIR LA DIM. ########

distTrafico_MDS_21<-as.data.frame(as.matrix(
   dist(mds_coord2$points)))

Vecinos4<-sapply(1:NCiudades_Restantes,function(i){
   rownames(distTrafico_MDS_21)[
      which(distTrafico_MDS_21[i,]==min(
         distTrafico_MDS_21[i,distTrafico_MDS_21[i,]!=0]))]})
Vecinos4<-as.data.frame(Vecinos4)
rownames(Vecinos4)<-rownames(distTrafico_MDS_21)

posibles_parejas2<-empareja(Vecinos4,NCiudades_Restantes)
parejas4<-parejas3
for (i in 1:dim(posibles_parejas2)[1]){
   if(
      length(which(parejas3==
                      posibles_parejas2[i,1]))==0 &
         length(which(parejas3==
                         posibles_parejas2[i,2]))==0)
      parejas4<-rbind(parejas4,posibles_parejas2[i,])
}
rownames(parejas4)<-c(1:dim(parejas4)[1])
nuevas_parejas3<-parejas4[-c(1:dim(parejas3)[1]),]
nuevas_parejas3

   parejas4<-rbind(parejas3,nuevas_parejas3)
   
   GMAs_Test<-NULL
   GMAs_Ctrl<-NULL
   for (k in parejas4[,1]){
      GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
   }
   for (k in parejas4[,2]){
      GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
   }
   mismoGMA<-GMAs_Test==GMAs_Ctrl
   which(mismoGMA==T)
   (parejas4<-parejas4[which(mismoGMA==F),])
   
# Las 2 parejas que podrian añadirse estan cada una en
   # una misma GMA con lo que seguimos con 19


######## VECINOS + PROXIMOS Y PAREJAS en las ciudades restantes ########

ciudades3<-sort(array_parejas3)
ciudades_Restantes2<-ciudades
for (i in 1:length(array_parejas3)){
   ciudades_Restantes2<-
      ciudades_Restantes2[which(
         ciudades_Restantes2!=ciudades3[i])]
}
NCiudades_Restantes2<-length(ciudades_Restantes2)

distTrafico_13<-distTrafico[ciudades_Restantes2,
                            ciudades_Restantes2]

Vecinos4<-sapply(1:NCiudades_Restantes2,function(i){
   rownames(distTrafico_13)[
      which(distTrafico_13[i,]==min(
         distTrafico_13[i,distTrafico_13[i,]!=0]))]})
Vecinos4<-as.data.frame(Vecinos4)
rownames(Vecinos4)<-rownames(distTrafico_13)

nuevas_parejas3<-empareja(Vecinos4,NCiudades_Restantes2)

   parejas4<-rbind(parejas3,nuevas_parejas3)
   
   GMAs_Test<-NULL
   GMAs_Ctrl<-NULL
   for (k in parejas4[,1]){
      GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
   }
   for (k in parejas4[,2]){
      GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
   }
   mismoGMA<-GMAs_Test==GMAs_Ctrl
   which(mismoGMA==T)
   (parejas4<-parejas4[which(mismoGMA==F),])

   p1<-parejas4[1,]
   p2<-parejas4[c(2:dim(parejas4)[1]),]
   num<-2^(dim(p2)[1])-1
   numBits<-dim(p2)[1]

lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico4<-lista[[1]]
d4<-lista[[2]]
Test4<-lista[[3]]
Ctrl4<-lista[[4]]
GTestTrafico4<-TSTrafico[,Test4]
GCtrlTrafico4<-TSTrafico[,Ctrl4]
GTestVisitas4<-TSVisitas[,Test4]
GCtrlVisitas4<-TSVisitas[,Ctrl4]
(cor(rowSums(GTestTrafico4),rowSums(GCtrlTrafico4)))
(cor(rowSums(GTestVisitas4),rowSums(GCtrlVisitas4)))
distTCVisitas4<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas4),rowSums(GCtrlVisitas4))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas4<-c(as.character(Test4),as.character(Ctrl4))
GTotalTrafico_rnd4<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd4<-GTotalTrafico_rnd4
for (i in 1:length(array_parejas4)){
   GTotalTrafico_rnd4[,i]<-
      TSTrafico[,array_parejas4[i]]
   GTotalVisitas_rnd4[,i]<-
      TSVisitas[,array_parejas4[i]]
   colnames(GTotalTrafico_rnd4)[i]<-
      array_parejas4[i]
   colnames(GTotalVisitas_rnd4)[i]<-
      array_parejas4[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas4),
           length(array_parejas4)/2,replace=FALSE) 
v2<-c(1:length(array_parejas4))[-v1]
GTestTrafico_rnd4<-GTotalTrafico_rnd4[v1]
GCtrlTrafico_rnd4<-GTotalTrafico_rnd4[v2]
GTestVisitas_rnd4<-GTotalVisitas_rnd4[v1]
GCtrlVisitas_rnd4<-GTotalVisitas_rnd4[v2]
distTCTrafico_rnd4<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd4),
         rowSums(GCtrlTrafico_rnd4))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd4),
    rowSums(GCtrlTrafico_rnd4))
distTCTrafico_rnd4
distTCTrafico4
distTCVisitas_rnd4<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd4),
         rowSums(GCtrlVisitas_rnd4))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd4),
    rowSums(GCtrlVisitas_rnd4))
distTCVisitas_rnd4
distTCVisitas4


######## REPRESENTACION ########

representa(GTestTrafico4,GCtrlTrafico4,
           GTestTrafico_rnd4,GCtrlTrafico_rnd4,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas4,GCtrlVisitas4,
           GTestVisitas_rnd4,GCtrlVisitas_rnd4,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)

mds_coord3<-cmdscale(distTrafico_13,eig=T,x.ret=T,add=F)
mds_coord3$GOF
max(mds_coord3$points[,1])
min(mds_coord3$points[,1])
max(mds_coord3$points[,2])
min(mds_coord3$points[,2])
plot(NULL,xlim=c(-.3,0.9),ylim=c(-0.2,0.1),asp=1,axes=T,lty=2)
s.label(mds_coord3$points,xax=1,yax=2,
        label=row.names(distTrafico_13),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.85),ylim=c(-0.2,0.1),cpoint=1,add.plot=T)
s.label(mds_coord3$points,xax=1,yax=2,
        label=row.names(distTrafico_13),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.3),ylim=c(-0.2,0.15),cpoint=1)


######## VECINOS Y PAREJAS AL REDUCIR LA DIM. ########

distTrafico_MDS_13<-as.data.frame(as.matrix(
   dist(mds_coord3$points)))

Vecinos5<-sapply(1:NCiudades_Restantes2,function(i){
   rownames(distTrafico_MDS_13)[
      which(distTrafico_MDS_13[i,]==min(
         distTrafico_MDS_13[i,distTrafico_MDS_13[i,]!=0]))]})
Vecinos5<-as.data.frame(Vecinos5)
rownames(Vecinos5)<-rownames(distTrafico_MDS_13)

posibles_parejas3<-empareja(Vecinos5,NCiudades_Restantes2)
parejas5<-parejas4
for (i in 1:dim(posibles_parejas3)[1]){
   if(
      length(which(parejas4==
                      posibles_parejas3[i,1]))==0 &
         length(which(parejas4==
                         posibles_parejas3[i,2]))==0)
      parejas5<-rbind(parejas5,posibles_parejas3[i,])
}
rownames(parejas5)<-c(1:dim(parejas5)[1])
nuevas_parejas4<-parejas5[-c(1:dim(parejas4)[1]),]
nuevas_parejas4

   parejas5<-rbind(parejas4,nuevas_parejas4)
   
   GMAs_Test<-NULL
   GMAs_Ctrl<-NULL
   for (k in parejas5[,1]){
      GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
   }
   for (k in parejas5[,2]){
      GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
   }
   mismoGMA<-GMAs_Test==GMAs_Ctrl
   which(mismoGMA==T)
   (parejas5<-parejas5[which(mismoGMA==F),])
   
   p1<-parejas5[1,]
   p2<-parejas5[c(2:dim(parejas5)[1]),]
   num<-2^(dim(p2)[1])-1
   numBits<-dim(p2)[1]

lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico5<-lista[[1]]
d5<-lista[[2]]
Test5<-lista[[3]]
Ctrl5<-lista[[4]]
GTestTrafico5<-TSTrafico[,Test5]
GCtrlTrafico5<-TSTrafico[,Ctrl5]
GTestVisitas5<-TSVisitas[,Test5]
GCtrlVisitas5<-TSVisitas[,Ctrl5]
(cor(rowSums(GTestTrafico5),rowSums(GCtrlTrafico5)))
(cor(rowSums(GTestVisitas5),rowSums(GCtrlVisitas5)))
distTCVisitas5<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas5),rowSums(GCtrlVisitas5))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas5<-c(as.character(Test5),as.character(Ctrl5))
GTotalTrafico_rnd5<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd5<-GTotalTrafico_rnd5
for (i in 1:length(array_parejas5)){
   GTotalTrafico_rnd5[,i]<-
      TSTrafico[,array_parejas5[i]]
   GTotalVisitas_rnd5[,i]<-
      TSVisitas[,array_parejas5[i]]
   colnames(GTotalTrafico_rnd5)[i]<-
      array_parejas5[i]
   colnames(GTotalVisitas_rnd5)[i]<-
      array_parejas5[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas5),
           length(array_parejas5)/2,replace=FALSE) 
v2<-c(1:length(array_parejas5))[-v1]
GTestTrafico_rnd5<-GTotalTrafico_rnd5[v1]
GCtrlTrafico_rnd5<-GTotalTrafico_rnd5[v2]
GTestVisitas_rnd5<-GTotalVisitas_rnd5[v1]
GCtrlVisitas_rnd5<-GTotalVisitas_rnd5[v2]
distTCTrafico_rnd5<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd5),
         rowSums(GCtrlTrafico_rnd5))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd5),
    rowSums(GCtrlTrafico_rnd5))
distTCTrafico_rnd5
distTCTrafico5
distTCVisitas_rnd5<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd5),
         rowSums(GCtrlVisitas_rnd5))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd5),
    rowSums(GCtrlVisitas_rnd5))
distTCVisitas_rnd5
distTCVisitas5


######## REPRESENTACION ########

representa(GTestTrafico5,GCtrlTrafico5,
           GTestTrafico_rnd5,GCtrlTrafico_rnd5,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas5,GCtrlVisitas5,
           GTestVisitas_rnd5,GCtrlVisitas_rnd5,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)


######## VECINOS + PROXIMOS Y PAREJAS en las ciudades restantes ########

ciudades4<-sort(array_parejas5)
ciudades_Restantes3<-ciudades
for (i in 1:length(array_parejas5)){
   ciudades_Restantes3<-
      ciudades_Restantes3[which(
         ciudades_Restantes3!=ciudades4[i])]
}
NCiudades_Restantes3<-length(ciudades_Restantes3)

distTrafico_3<-distTrafico[ciudades_Restantes3,
                            ciudades_Restantes3]

Vecinos6<-sapply(1:NCiudades_Restantes3,function(i){
   rownames(distTrafico_3)[
      which(distTrafico_3[i,]==min(
         distTrafico_3[i,distTrafico_3[i,]!=0]))]})
Vecinos6<-as.data.frame(Vecinos6)
rownames(Vecinos6)<-rownames(distTrafico_3)

nuevas_parejas5<-empareja(Vecinos6,NCiudades_Restantes3)

ciudades_Restantes3<-ciudades_Restantes3[
   ciudades_Restantes3!="Fuenlabrada" &
      ciudades_Restantes3!="Parla" &
      ciudades_Restantes3!="Ourense" &
      ciudades_Restantes3!="Pontevedra"]
NCiudades_Restantes3<-length(ciudades_Restantes3)

distTrafico_3<-distTrafico[ciudades_Restantes3,
                           ciudades_Restantes3]

Vecinos6<-sapply(1:NCiudades_Restantes3,function(i){
   rownames(distTrafico_3)[
      which(distTrafico_3[i,]==min(
         distTrafico_3[i,distTrafico_3[i,]!=0]))]})
Vecinos6<-as.data.frame(Vecinos6)
rownames(Vecinos6)<-rownames(distTrafico_3)

nuevas_parejas5<-empareja(Vecinos6,NCiudades_Restantes3)

parejas6<-rbind(parejas5,nuevas_parejas5)

GMAs_Test<-NULL
GMAs_Ctrl<-NULL
for (k in parejas6[,1]){
   GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
}
for (k in parejas6[,2]){
   GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
}
mismoGMA<-GMAs_Test==GMAs_Ctrl
which(mismoGMA==T)
(parejas6<-parejas6[which(mismoGMA==F),])

# p1<-parejas6[1,]
# p2<-parejas6[c(2:dim(parejas6)[1]),]
p1<-cbind(as.character(Test5),as.character(Ctrl5))
p2<-parejas6[c(22:23),]
num<-2^(dim(p2)[1])-1
numBits<-dim(p2)[1]
lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico6<-lista[[1]]
d6<-lista[[2]]
Test6<-lista[[3]]
Ctrl6<-lista[[4]]
GTestTrafico6<-TSTrafico[,Test6]
GCtrlTrafico6<-TSTrafico[,Ctrl6]
GTestVisitas6<-TSVisitas[,Test6]
GCtrlVisitas6<-TSVisitas[,Ctrl6]
(cor(rowSums(GTestTrafico6),rowSums(GCtrlTrafico6)))
(cor(rowSums(GTestVisitas6),rowSums(GCtrlVisitas6)))
distTCVisitas6<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas6),rowSums(GCtrlVisitas6))),
   "DWT",diag=T)/max_distVisitas
plot(1:(num+1),d6,type="l",xlab="#iteración",
     ylab="distancia",main="Distancia entre Test y Ctrl")
plot(1:(num+1),sort(d6,decreasing=T),type="l",
     xlab="#iteración",ylab="distancia",
     main="Distancia entre Test y Ctrl")


######## SELECCION ALEATORIA ########

array_parejas6<-c(parejas6[,1],as.character(parejas6[,2]))
GTotalTrafico_rnd6<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd6<-GTotalTrafico_rnd6
for (i in 1:length(array_parejas6)){
   GTotalTrafico_rnd6[,i]<-
      TSTrafico[,array_parejas6[i]]
   GTotalVisitas_rnd6[,i]<-
      TSVisitas[,array_parejas6[i]]
   colnames(GTotalTrafico_rnd6)[i]<-
      array_parejas6[i]
   colnames(GTotalVisitas_rnd6)[i]<-
      array_parejas6[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas6),
           length(array_parejas6)/2,replace=FALSE) 
v2<-c(1:length(array_parejas6))[-v1]
GTestTrafico_rnd6<-GTotalTrafico_rnd6[v1]
GCtrlTrafico_rnd6<-GTotalTrafico_rnd6[v2]
GTestVisitas_rnd6<-GTotalVisitas_rnd6[v1]
GCtrlVisitas_rnd6<-GTotalVisitas_rnd6[v2]
distTCTrafico_rnd6<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd6),
         rowSums(GCtrlTrafico_rnd6))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd6),
    rowSums(GCtrlTrafico_rnd6))
distTCTrafico_rnd6
distTCTrafico6
distTCVisitas_rnd6<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd6),
         rowSums(GCtrlVisitas_rnd6))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd6),
    rowSums(GCtrlVisitas_rnd6))
distTCVisitas_rnd6
distTCVisitas6


######## REPRESENTACION ########

representa(GTestTrafico6,GCtrlTrafico6,
           GTestTrafico_rnd6,GCtrlTrafico_rnd6,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas6,GCtrlVisitas6,
           GTestVisitas_rnd6,GCtrlVisitas_rnd6,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)

mds_coord4<-cmdscale(distTrafico_3,eig=T,x.ret=T,add=F)
mds_coord4$GOF
max(mds_coord4$points[,1])
min(mds_coord4$points[,1])
max(mds_coord4$points[,2])
min(mds_coord4$points[,2])
plot(NULL,xlim=c(-.3,0.9),ylim=c(-0.2,0.1),asp=1,axes=T,lty=2)
s.label(mds_coord4$points,xax=1,yax=2,
        label=row.names(distTrafico_3),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.85),ylim=c(-0.2,0.1),cpoint=1,add.plot=T)
s.label(mds_coord4$points,xax=1,yax=2,
        label=row.names(distTrafico_3),clabel = 0.6,
        boxes=F,xlim=c(-.2,0.3),ylim=c(-0.2,0.15),cpoint=1)


######## VECINOS Y PAREJAS AL REDUCIR LA DIM. ########

distTrafico_MDS_11<-as.data.frame(as.matrix(
   dist(mds_coord4$points)))

Vecinos7<-sapply(1:NCiudades_Restantes3,function(i){
   rownames(distTrafico_MDS_11)[
      which(distTrafico_MDS_11[i,]==min(
         distTrafico_MDS_11[i,distTrafico_MDS_11[i,]!=0]))]})
Vecinos7<-as.data.frame(Vecinos7)
rownames(Vecinos7)<-rownames(distTrafico_MDS_11)

posibles_parejas4<-empareja(Vecinos7,NCiudades_Restantes3)
parejas7<-parejas6
for (i in 1:dim(posibles_parejas4)[1]){
   if(
      length(which(parejas6==
                      posibles_parejas4[i,1]))==0 &
         length(which(parejas6==
                         posibles_parejas4[i,2]))==0)
      parejas7<-rbind(parejas7,posibles_parejas4[i,])
}
rownames(parejas7)<-c(1:dim(parejas7)[1])
nuevas_parejas6<-parejas7[-c(1:dim(parejas6)[1]),]
nuevas_parejas6

parejas7<-rbind(parejas6,nuevas_parejas6)

GMAs_Test<-NULL
GMAs_Ctrl<-NULL
for (k in parejas7[,1]){
   GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
}
for (k in parejas7[,2]){
   GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
}
mismoGMA<-GMAs_Test==GMAs_Ctrl
which(mismoGMA==T)
(parejas7<-parejas7[which(mismoGMA==F),])

# p1<-parejas7[1,]
# p2<-parejas7[c(2:dim(parejas7)[1]),]
p1<-cbind(as.character(Test6),as.character(Ctrl6))
p2<-parejas7[24,]
num<-2^(dim(p2)[1])-1
numBits<-dim(p2)[1]
lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico7<-lista[[1]]
d7<-lista[[2]]
Test7<-lista[[3]]
Ctrl7<-lista[[4]]
GTestTrafico7<-TSTrafico[,Test7]
GCtrlTrafico7<-TSTrafico[,Ctrl7]
GTestVisitas7<-TSVisitas[,Test7]
GCtrlVisitas7<-TSVisitas[,Ctrl7]
(cor(rowSums(GTestTrafico7),rowSums(GCtrlTrafico7)))
(cor(rowSums(GTestVisitas7),rowSums(GCtrlVisitas7)))
distTCVisitas7<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas7),rowSums(GCtrlVisitas7))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas7<-c(as.character(Test7),as.character(Ctrl7))
GTotalTrafico_rnd7<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd7<-GTotalTrafico_rnd7
for (i in 1:length(array_parejas7)){
   GTotalTrafico_rnd7[,i]<-
      TSTrafico[,array_parejas7[i]]
   GTotalVisitas_rnd7[,i]<-
      TSVisitas[,array_parejas7[i]]
   colnames(GTotalTrafico_rnd7)[i]<-
      array_parejas7[i]
   colnames(GTotalVisitas_rnd7)[i]<-
      array_parejas7[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas7),
           length(array_parejas7)/2,replace=FALSE) 
v2<-c(1:length(array_parejas2))[-v1]
GTestTrafico_rnd7<-GTotalTrafico_rnd7[v1]
GCtrlTrafico_rnd7<-GTotalTrafico_rnd7[v2]
GTestVisitas_rnd7<-GTotalVisitas_rnd7[v1]
GCtrlVisitas_rnd7<-GTotalVisitas_rnd7[v2]
distTCTrafico_rnd7<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd7),
         rowSums(GCtrlTrafico_rnd7))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd7),
    rowSums(GCtrlTrafico_rnd7))
distTCTrafico_rnd7
distTCTrafico7
distTCVisitas_rnd7<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd7),
         rowSums(GCtrlVisitas_rnd7))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd7),
    rowSums(GCtrlVisitas_rnd7))
distTCVisitas_rnd7
distTCVisitas7


######## REPRESENTACION ########

representa(GTestTrafico7,GCtrlTrafico7,
           GTestTrafico_rnd7,GCtrlTrafico_rnd7,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas7,GCtrlVisitas7,
           GTestVisitas_rnd7,GCtrlVisitas_rnd7,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)


######## VECINOS + PROXIMOS Y PAREJAS en las ciudades restantes ########

ciudades5<-sort(array_parejas7)
ciudades_Restantes4<-ciudades
for (i in 1:length(array_parejas7)){
   ciudades_Restantes4<-
      ciudades_Restantes4[which(
         ciudades_Restantes4!=ciudades5[i])]
}
NCiudades_Restantes4<-length(ciudades_Restantes4)

distTrafico_4<-distTrafico[ciudades_Restantes4,
                           ciudades_Restantes4]

Vecinos8<-sapply(1:NCiudades_Restantes4,function(i){
   rownames(distTrafico_4)[
      which(distTrafico_4[i,]==min(
         distTrafico_4[i,distTrafico_4[i,]!=0]))]})
Vecinos8<-as.data.frame(Vecinos8)
rownames(Vecinos8)<-rownames(distTrafico_4)

nuevas_parejas7<-empareja(Vecinos8,NCiudades_Restantes4)

ciudades_Restantes4<-ciudades_Restantes4[
   ciudades_Restantes4!="Fuenlabrada" &
      ciudades_Restantes4!="Parla" &
      ciudades_Restantes4!="Ourense" &
      ciudades_Restantes4!="Pontevedra" &
      ciudades_Restantes4!="Coslada" &
      ciudades_Restantes4!="Guadalajara"]
NCiudades_Restantes4<-length(ciudades_Restantes4)

distTrafico_5<-distTrafico[ciudades_Restantes4,
                           ciudades_Restantes4]

Vecinos8<-sapply(1:NCiudades_Restantes4,function(i){
   rownames(distTrafico_5)[
      which(distTrafico_5[i,]==min(
         distTrafico_5[i,distTrafico_5[i,]!=0]))]})
Vecinos8<-as.data.frame(Vecinos8)
rownames(Vecinos8)<-rownames(distTrafico_5)

nuevas_parejas7<-empareja(Vecinos8,NCiudades_Restantes4)

parejas8<-rbind(parejas7,nuevas_parejas7)

GMAs_Test<-NULL
GMAs_Ctrl<-NULL
for (k in parejas8[,1]){
   GMAs_Test<-cbind(GMAs_Test,GMAs[GMAs[,1]==k,2])   
}
for (k in parejas8[,2]){
   GMAs_Ctrl<-cbind(GMAs_Ctrl,GMAs[GMAs[,1]==k,2])   
}
mismoGMA<-GMAs_Test==GMAs_Ctrl
which(mismoGMA==T)
(parejas8<-parejas8[which(mismoGMA==F),])

# p1<-parejas8[1,]
# p2<-parejas8[c(2:dim(parejas8)[1]),]
p1<-cbind(as.character(Test7),as.character(Ctrl7))
p2<-parejas8[25,]
num<-2^(dim(p2)[1])-1
numBits<-dim(p2)[1]
lista<-optimiza(p1,p2,num,numBits,max_distTrafico)
distTCTrafico8<-lista[[1]]
d8<-lista[[2]]
Test8<-lista[[3]]
Ctrl8<-lista[[4]]
GTestTrafico8<-TSTrafico[,Test8]
GCtrlTrafico8<-TSTrafico[,Ctrl8]
GTestVisitas8<-TSVisitas[,Test8]
GCtrlVisitas8<-TSVisitas[,Ctrl8]
(cor(rowSums(GTestTrafico8),rowSums(GCtrlTrafico8)))
(cor(rowSums(GTestVisitas8),rowSums(GCtrlVisitas8)))
distTCVisitas8<-diss(as.data.frame(cbind(
   rowSums(GTestVisitas8),rowSums(GCtrlVisitas8))),
   "DWT",diag=T)/max_distVisitas


######## SELECCION ALEATORIA ########

array_parejas8<-c(parejas8[,1],as.character(parejas8[,2]))
GTotalTrafico_rnd8<-data.frame(
   row.names=row.names(TSTrafico))
GTotalVisitas_rnd8<-GTotalTrafico_rnd8
for (i in 1:length(array_parejas8)){
   GTotalTrafico_rnd8[,i]<-
      TSTrafico[,array_parejas8[i]]
   GTotalVisitas_rnd8[,i]<-
      TSVisitas[,array_parejas8[i]]
   colnames(GTotalTrafico_rnd8)[i]<-
      array_parejas8[i]
   colnames(GTotalVisitas_rnd8)[i]<-
      array_parejas8[i]
}
set.seed(123456789)
v1<-sample(length(array_parejas8),
           length(array_parejas8)/2,replace=FALSE) 
v2<-c(1:length(array_parejas8))[-v1]
GTestTrafico_rnd8<-GTotalTrafico_rnd8[v1]
GCtrlTrafico_rnd8<-GTotalTrafico_rnd8[v2]
GTestVisitas_rnd8<-GTotalVisitas_rnd8[v1]
GCtrlVisitas_rnd8<-GTotalVisitas_rnd8[v2]
distTCTrafico_rnd8<-diss(as.data.frame(
   cbind(rowSums(GTestTrafico_rnd8),
         rowSums(GCtrlTrafico_rnd8))),"DWT",
   diag=T)/max_distTrafico
cor(rowSums(GTestTrafico_rnd8),
    rowSums(GCtrlTrafico_rnd8))
distTCTrafico_rnd8
distTCTrafico8
distTCVisitas_rnd8<-diss(as.data.frame(
   cbind(rowSums(GTestVisitas_rnd8),
         rowSums(GCtrlVisitas_rnd8))),"DWT",
   diag=T)/max_distVisitas
cor(rowSums(GTestVisitas_rnd8),
    rowSums(GCtrlVisitas_rnd8))
distTCVisitas_rnd8
distTCVisitas8


######## REPRESENTACION ########

representa(GTestTrafico8,GCtrlTrafico8,
           GTestTrafico_rnd8,GCtrlTrafico_rnd8,
           GTestTrafico_rnd_TOTAL,
           GCtrlTrafico_rnd_TOTAL,
           GTestVisitas8,GCtrlVisitas8,
           GTestVisitas_rnd8,GCtrlVisitas_rnd8,
           GTestVisitas_rnd_TOTAL,
           GCtrlVisitas_rnd_TOTAL)

