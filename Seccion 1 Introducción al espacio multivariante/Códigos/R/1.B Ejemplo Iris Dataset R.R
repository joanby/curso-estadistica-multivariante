
##################################################################################################################
##################################################################################################################
# IRIS DATASET - Introduccion al espacio multivariante
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Abrir R-studio.
##################################################################################################################

##################################################################################################################
# Abrir el dataset de iris
##################################################################################################################

?iris #info sobre el dataset

#Cargar los datos
Y <- as.data.frame(iris)
Y

##################################################################################################################
# Definir como n.Y y p.Y el numero de flores y el numero de variables.
##################################################################################################################

dim(Y)

n.Y <- nrow(Y)
n.Y
p.Y <- ncol(Y)
p.Y

##################################################################################################################
##################################################################################################################
# Visualización
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Obtener los boxplots (solo para las variables numéricas)
##################################################################################################################

par(mfrow=c(2,2))
sapply(seq(1,4),function(j)boxplot(Y[,j],main=colnames(Y)[j],xlab="",col="blue"))

##################################################################################################################
# Obtener los boxplots (solo para las variables numéricas) separados por especie para cada variable
##################################################################################################################

sapply(seq(1,4),function(j)boxplot(Y[,j]~Y[,5],main=colnames(Y)[j],xlab="",col="blue"))


##################################################################################################################
# Obtener un bagplot 
##################################################################################################################

install.packages("aplpack")
library(aplpack)
bagplot(Y$Sepal.Length,Y$Sepal.Width,xlab="Sepal.Length", ylab="Sepal.Width", main="Bagplot")


##################################################################################################################
# Histogramas para las variables cuantitativas con método óptimo para determinar el ancho de las barras (binwidth)
##################################################################################################################

par(mfrow=c(2,2))
sapply(seq(1,4),function(j)hist(Y[,j],main=colnames(Y)[j],xlab="",col="blue",breaks = "Sturges"))

##################################################################################################################
# Histogramas para la variable Petal Length para cada subgrupo en Especie (setosa, versicolor, virginica) 
##################################################################################################################

par(mfrow=c(1,3))
hist(Y[1:50,3],main="Petal Length (setosa)",xlab="",col="blue",breaks = "Sturges")
hist(Y[51:100,3],main="Petal Length (versicolor)",xlab="",col="blue",breaks = "Sturges")
hist(Y[101:150,3],main="Petal Length (virginica)",xlab="",col="blue",breaks = "Sturges")


##################################################################################################################
# Densidad Kernel para las variables con el ancho de banda (bandwidth) óptimo y el Kernel Gaussiano 
##################################################################################################################

par(mfrow=c(2,2))
sapply(seq(1,4),function(j)plot(density(Y[,j],kernel="gaussian"),main=colnames(Y)[j],xlab="",col="blue",lwd=2))


##################################################################################################################
# Densidad Kernel para las variables con el ancho de banda (bandwidth) óptimo y el Kernel Gaussiano y el Epanechnikov
##################################################################################################################

par(mfrow=c(2,2))
sapply(seq(1,4),function(j){
  plot(density(Y[,j],kernel="gaussian"),main=colnames(Y)[j],xlab="",col="blue",lwd=2)
  lines(density(Y[,j],kernel="epanechnikov"),main=colnames(Y)[j],xlab="",col="green",lwd=2)}
)


##################################################################################################################
# Densidad Kernel
##################################################################################################################
install.packages("KernSmooth")
install.packages("MASS")
install.packages("ks")
library(MASS)
library(ks)


bivkde=kde2d(Y$Sepal.Length, Y$Sepal.Width)
library(rgl)
col1=rainbow(length(bivkde$z))[rank(bivkde$z)]
persp3d(bivkde,col=col1)
play3d( spin3d( axis = c(0, 0, 1),    rpm = 7),duration = 100 )


##################################################################################################################
# Scatterplot de Sepal.Length y Petal.Length
##################################################################################################################

attach(Y)
par(mfrow=c(1,1)) # Volver a definir una ventana normal
plot(Sepal.Length,Petal.Length,pch=19,xlab="Sepal.Length",ylab="Petal.Length")

##################################################################################################################
# 3D-Scatterplot de Sepal.Length, Petal.Length y Petal.Width
##################################################################################################################

library(scatterplot3d)
scatterplot3d(Sepal.Length,Petal.Length,Petal.Width,pch=19,color=c("blue","green","orange")[Y[,5]]) # 3D scatterplot
scatterplot3d(Sepal.Length,Petal.Length,Petal.Width,pch=19,color=c("blue","green","orange")[Y[,5]],type="h") # 3D scatterplot

library(rgl)
open3d() # Abrir una ventana de 3 dimensiones.
plot3d(Sepal.Length,Petal.Length,Petal.Width,size=5,col=c("blue","green","orange")[Y[,5]]) # Las variables se pueden rotar

library(plotly)
plot_ly(Y,x=~Sepal.Length,y=~Petal.Length,z=~Petal.Width,color=c("blue","green","orange")[Y[,5]]) # Otra posibilidad

library(plot3Drgl)
plot3d( Y$Sepal.Length, Y$Sepal.Width, Y$Petal.Length, col = Y$Petal.Width,  type = "s",   radius = 0.1)
play3d( spin3d( axis = c(0, 0, 1),    rpm = 7),duration = 100 )


##################################################################################################################
# Scatterplot múltiple, dividido por subgrupos en cada gráfico
##################################################################################################################

pairs(Y[,1:4],pch=19,col=c("blue","green","orange")[Y[,5]])


##################################################################################################################
# Coordenadas paralelas
##################################################################################################################

parcoord(Y[,1:4],col=c("blue","green","orange")[Y[,5]],var.label = TRUE)





##################################################################################################################
##################################################################################################################
# Medidas descriptivas multivariantes
##################################################################################################################
##################################################################################################################




##################################################################################################################
# El vector de medias
##################################################################################################################

mu.Y <- colMeans(Y[,1:4])
mu.Y





##################################################################################################################
# Profundidad de Tukey
##################################################################################################################



library(ddalpha)

depth.Y <- depth.halfspace(Y[,1:4],Y[,1:4],num.directions=100000,seed=1) # Obtener la solución aproximada basada en 100000 proyecciones
sort.depth.Y <- sort(depth.Y,decreasing=TRUE,index.return=TRUE) # Ordenar las profundidades

depth.Y.sort <- sort.depth.Y$x # Las profundidades ordenadas, desde la más profunda hasta la menos profunda, con respecto al centro
depth.Y.sort

depth.Y.sort.index <- sort.depth.Y$ix # Las filas asociadas a esas profundidades ya ordenadas
depth.Y.sort.index

# El indice de la primera es la más profunda, es la mediana
Y[,1:4][depth.Y.sort.index[1],]
median=sort.depth.Y$ix[1]

par(mfrow=c(1,1))
plot(Y$Sepal.Length, Y$Sepal.Width,  xlab="Sepal.Length ", ylab="Sepal.Width ", pch=19,col=c("blue","green","orange")[Y[,5]])
points(Y$Sepal.Length[median],Y$Sepal.Width[median], col = "red", pch=19,bg="red",lwd=2,cex=2)



data=Y[,1:4]
depths1 <- depth.halfspace(data[1:50,],data=data[1:50,])
depths2 <- depth.halfspace(data[51:100,],data=data[51:100,])
depths3 <- depth.halfspace(data[101:150,],data=data[101:150,])
vector1 <-sort(depths1, decreasing=TRUE, index.return=TRUE) 
median1=vector1$ix[1]
vector2 <-sort(depths2, decreasing=TRUE, index.return=TRUE) 
median2=vector2$ix[1]+50
vector3 <-sort(depths3, decreasing=TRUE, index.return=TRUE) 
median3=vector3$ix[1]+100



plot(Y$Sepal.Length, Y$Sepal.Width,
     xlab="Sepal.Length ", ylab="Sepal.Width ", pch=19,col=c("blue","green","magenta")[Y[,5]])
points(Y$Sepal.Length[median1],Y$Sepal.Width[median1], col = "green", pch=19,bg="green",lwd=2,cex=2)
points(Y$Sepal.Length[median2],Y$Sepal.Width[median2], col = "red", pch=19,bg="red",lwd=2,cex=2)
points(Y$Sepal.Length[median3],Y$Sepal.Width[median3], col = "orange", pch=19,bg="orange",lwd=2,cex=2)



##################################################################################################################
# La matriz de covarianza y de correlaciones con sus respectivos autovalores.
##################################################################################################################

S.Y <- cov(Y[,1:4])
S.Y

eigen(S.Y) # Autovalores y autovectores de S
sum(eigen(S.Y)$values) # Traza de la matriz S como la suma de autovalores 
det(S.Y) # Determinante de S

R.Y <- cor(Y[,1:4])
R.Y

eigen(R.Y) # Autovalores y autovectores de R
sum(eigen(R.Y)$values) # Traza de la matriz R como la suma de autovalores 
det(R.Y) # Determinante de R


##################################################################################################################
# Transformaciones lineales
##################################################################################################################

C <- matrix(NA,nrow=4,ncol=2)
C[,1] <- c(1,0,1,0)
C[,2] <- c(0,1,0,1)

Ytrans <- as.matrix(Y[,1:4]) %*% C
Ytrans

colMeans(Ytrans)
cov(Ytrans)

t(C) %*% mu.Y
t(C) %*% S.Y %*% C


##################################################################################################################
# Estandarizaciones
##################################################################################################################

# Scatterplot matrix
pairs(Y[,1:4],pch=19,col=c("blue","green","orange")[Y[,5]])


# Univariante
sY <- scale(Y[,1:4])
pairs(sY,pch=19,col=c("blue","green","orange")[Y[,5]])

# Multivariante 
S.Y <- cov(Y[,1:4])
S.Y
iS.Y <- solve(S.Y)
iS.Y
e <- eigen(iS.Y)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V)
Ytil <- scale(Y[,1:4],scale = FALSE)
SY <- Ytil %*% B
colMeans(SY)
cov(SY)
pairs(SY,pch=19,col=c("blue","green","orange")[Y[,5]])

