# Tarrer-5-Bioestadística
## setwd("C:/Users/Laboratorio/Desktop/BE Taller 5")
getwd()
list.files()
data_DB<-read.csv("Datos_Biometricos.csv",header=TRUE,sep=";")
data_DB
names(data_DB)
head(data_DB)
tail(data_DB)
dim(data_DB)
hist((data_DB$Cont_Hombros),xlab="Contorno de hombros (cm)",ylab="Frecuencia",main="Histograma de contorno de hombros",col="red")
qqnorm(data_DB$Cont_Hombros)
qqline(data_DB$Cont_Hombros)
#H0: los datos de cont hombros se distribuyen normalmente
#H1: los datos de cont hombros NO se distribuyen normalmente
shapiro.test(data_DB$Cont_Hombros)
#Se rechaza la hipótesis nula
dataF<-data_DB$Sexo=="0"
dataF
dataM<-data_DB$Sexo=="1"
dataM
prom_Cont_hom_F<-mean(data_DB$Cont_Hombros[dataF])
prom_Cont_hom_F
desv_Cont_hom_F<-sd(data_DB$Cont_Hombros[dataF])
desv_Cont_hom_F
prom_Cont_hom_M<-mean(data_DB$Cont_Hombros[dataM])
prom_Cont_hom_M
desv_Cont_hom_M<-sd(data_DB$Cont_Hombros[dataM])
desv_Cont_hom_M
par(mfrow=c(2,1))
hist(data_DB$Cont_Hombros[dataF],col="pink",ylab="Probabilidad",xlab="Contorno de hombros (cm)",main="Distribucion del contorno alrededor de los hombros: Mujeres",probability = TRUE)
x<-80:135
x
y<-dnorm(x=x,mean=prom_Cont_hom_F,sd=desv_Cont_hom_F)
y
lines(x=x,y=y,col="red",lwd=3)
hist(data_DB$Cont_Hombros[dataM],col="blue",ylab="Probabilidad",xlab="Contorno de hombros (cm)",main="Distribucion del contorno alrededor de los hombros: Hombres",probability = TRUE)
x<-95:140
x
y<-dnorm(x=x,mean=prom_Cont_hom_M,sd=desv_Cont_hom_M)
y
lines(x=x,y=y,col="red",lwd=3)
summary(data_DB$Cont_Hombros[dataF])
summary(data_DB$Cont_Hombros[dataM])
boxplot(Cont_Hombros~Sexo,data=data,col=c("red","blue")?????????????????????
rqqnorm(data_DB$Cont_Hombros[dataF],main="Normal Q-Q Plot - mujeres")
qqline(data_DB$Cont_Hombros[dataF],col="red",lwd=2)
#H0: los datos se distribuyen normalmente
#H1: los datos NO se distribuyen normalmente
shapiro.test(data_DB$Cont_Hombros[dataF])
qqnorm(data_DB$Cont_Hombros[dataM],main="Normal Q-Q Plot - hombres")
qqline(data_DB$Cont_Hombros[dataM],col="red",lwd=2)
#H0: los datos se distribuyen normalmente
#H1: los datos NO se distribuyen normalmente
shapiro.test(data_DB$Cont_Hombros[dataM])
 
prom_Altura_F<-mean(data_DB$Altura[dataF])
prom_Altura_F
desv_Altura_F<-sd(data_DB$Altura[dataF])
desv_Altura_F
prom_Altura_M<-mean(data_DB$Altura[dataM])
prom_Altura_M
desv_Altura_M<-sd(data_DB$Altura[dataM])
desv_Altura_M
par(mfrow=c(2,1))
hist(data_DB$Altura[dataF],col="pink",ylab="Probabilidad",xlab="Altura (cm)",main="Distribucion de la altura: mujeres",probability=TRUE,ylim=c(0,0.06))
x<-140:190
x
y<-dnorm(x=x,mean=prom_Altura_F,sd=desv_Altura_F)
y
lines(x=x,y=y,col="red",lwd=3)
hist(data_DB$Altura[dataM],col="blue",ylab="Probabilidad",xlab="Altura (cm)",main="Distribucion de la altura: hombres",probability=TRUE)
z<-150:205
z
y<-dnorm(x=z,mean=prom_Altura_M,sd=desv_Altura_M)
y
lines(x=z,y=y,col="red",lwd=3)
>summary(data$Altura[dataf])
>summary(data$Altura[datam])
>qqnorm(data$Altura[dataf],main="Normal Q-Q Plot - mujeres")
>qqline(data$Altura[dataf],col="red",lwd=2)
>qqnorm(data$Altura[datam],main="Normal Q-Q Plot - hombres")
>qqline(data$Altura[datam],col="red",lwd=2)
>shapiro.test(data$Altura[dataf])
shapiro.test(data$Altura[datam])
pnorm(q=182, mean=prom_Altura_f, sd=desv_Altura_f)
> 1-0.9955656
sum(data$Altura[dataf] > 182)/length(data$Altura[dataf])(data$Altura[dataf] > 182)/length(data$Altura[dataf])
1-0.003846154
norm<- pnorm(q=160, mean=prom_Altura_f, sd=desv_Altura_f)
actual<- sum(data$Altura[dataf] < 160)/length(data$Altura[dataf])
