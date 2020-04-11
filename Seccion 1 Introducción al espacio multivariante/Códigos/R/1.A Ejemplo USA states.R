##################################################################################################################
##################################################################################################################
# USA dataset - Introduccion al espacio multivariante
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Abrir R-studio.
##################################################################################################################



##################################################################################################################
# Abrir el dataset llamado state.x77
##################################################################################################################

?state.x77 #información sobre el dataset
#Cargar los datos
X <- as.data.frame(state.x77)
X

##################################################################################################################
# Definir como n.X y p.X el numero de estados y el numero de variables.
##################################################################################################################

dim(X)

n.X <- nrow(X)
n.X
p.X <- ncol(X)
p.X


##################################################################################################################
##################################################################################################################
# Visualización
##################################################################################################################
##################################################################################################################


##################################################################################################################
# Obtener los boxplots 
##################################################################################################################

par(mfrow=c(2,4))
sapply(seq(1,8),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="blue"))




##################################################################################################################
# Histogramas para la variable "Murder" con diferentes anchos de barra (binwidth) - opcion "breaks"
##################################################################################################################

par(mfrow=c(1,3))
hist(X[,5],main="Murder with h=1",xlab="",col="blue",freq=FALSE,breaks=seq(min(X[,5]),max(X[,5])+1,by=1))
hist(X[,5],main="Murder with h=2",xlab="",col="blue",freq=FALSE,breaks=seq(min(X[,5]),max(X[,5])+2,by=2))
hist(X[,5],main="Murder with h=3",xlab="",col="blue",freq=FALSE,breaks=seq(min(X[,5]),max(X[,5])+3,by=3))

##################################################################################################################
# Histogramas para todas las variables con ancho de barra "óptimo" - - opcion breaks=Sturges
##################################################################################################################

par(mfrow=c(2,4))
sapply(seq(1,8),function(j)hist(X[,j],main=colnames(X)[j],xlab="",col="blue",breaks = "Sturges"))


##################################################################################################################
# Densidad Kernel para las variables con el ancho de banda (bandwidth) óptimo y el Kernel Gaussiano 
##################################################################################################################

par(mfrow=c(2,4))
sapply(seq(1,8),function(j)plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="blue",lwd=2))

##################################################################################################################
# Densidad Kernel para las variables con el ancho de banda (bandwidth) óptimo y el Kernel Gaussiano y el Epanechnikov
##################################################################################################################

sapply(seq(1,8),function(j){
  plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="blue",lwd=2)
  lines(density(X[,j],kernel="epanechnikov"),main=colnames(X)[j],xlab="",col="green",lwd=2)}
)


##################################################################################################################
# Scatterplot de las variables Income y Life Expectancy.
##################################################################################################################

attach(X)

par(mfrow=c(1,1)) # Volver a definir una ventana normal
plot(Income,`Life Exp`,pch=19,col="blue",xlab="Income",ylab="Life expectancy")

##################################################################################################################
# 3D-Scatterplots de Income, Life Expectancy y Murder
##################################################################################################################

library(scatterplot3d)
scatterplot3d(Income,`Life Exp`,Murder,pch=19,color="blue") # 3D scatterplot
scatterplot3d(Income,`Life Exp`,Murder,pch=19,color="blue",type="h") # 3D scatterplot

install.packages("Rcpp")
library(rgl)
open3d() # Abrir una ventana gráfica en 3 dimensiones
plot3d(Income,`Life Exp`,Murder,size=5) # Se pueden rotar los ejes.

library(plotly)
plot_ly(X,x=~Income,y=~`Life Exp`,z=~Murder) # Otra posibilidad


##################################################################################################################
# Scatterplot múltiple de todas los pares de varaibles.
##################################################################################################################

pairs(X,pch=19,col="blue")



##################################################################################################################
# Coordenadas paralelas
##################################################################################################################

library(MASS)
parcoord(X,col="blue",var.label = TRUE)


##################################################################################################################
##################################################################################################################
# Medidas descriptivas multivariantes
##################################################################################################################
##################################################################################################################


##################################################################################################################
# El vector de medias 
##################################################################################################################

mu.X <- colMeans(X)
mu.X


##################################################################################################################
# Profundidad de Tukey
##################################################################################################################

library(ddalpha)

depth.X <- depth.halfspace(X,X,num.directions=100000,seed=1) # Obtener la solución aproximada basada en 100000 proyecciones
sort.depth.X <- sort(depth.X,decreasing=TRUE,index.return=TRUE) # Ordenar las profundidades

depth.X.sort <- sort.depth.X$x # Las profundidades ordenadas, desde la más profunda hasta la menos profunda, con respecto al centro
depth.X.sort

depth.X.sort.index <- sort.depth.X$ix # Las filas asociadas a esas profundidades ya ordenadas
depth.X.sort.index

# El indice de la primera es la más profunda, es la mediana

X[depth.X.sort.index[1],]

##################################################################################################################
# La matriz de covarianza y de correlaciones con sus respectivos autovalores.
##################################################################################################################

S.X <- cov(X)
S.X

eigen(S.X) # Autovalores y autovectores de S
sum(eigen(S.X)$values) # Traza de la matriz S como la suma de autovalores 
det(S.X) # Determinante de S

R.X <- cor(X)
R.X

eigen(R.X) # Autovalores y autovectores de R
sum(eigen(R.X)$values) # Traza de la matriz R como la suma de autovalores 
det(R.X) # Determinante de R


##################################################################################################################
# Estandarizaciones
##################################################################################################################

# Scatterplot matrix 
pairs(X,pch=19,col="blue")

# Univariante 
sX <- scale(X)
pairs(sX,pch=19,col="blue")

# Multivariante 
iS.X <- solve(S.X)
e <- eigen(iS.X)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V)
Xtil <- scale(X,scale = FALSE)
SX <- Xtil %*% B
colMeans(SX)
cov(SX)
pairs(SX,pch=19,col="blue")




