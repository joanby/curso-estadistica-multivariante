
##################################################################################################################
##################################################################################################################
# Inferencia
##################################################################################################################
##################################################################################################################


##################################################################################################################
##################################################################################################################
# Analisis de varianza multivariante con el dataset de Iris
##################################################################################################################
##################################################################################################################

rm(list = ls()) # Borrar todo

Y <- as.data.frame(iris)
Y

n <- dim(Y)[1]
n


X <- Y[,1:4]
p <- dim(X)[2]
p

G <- 3
n1 <- 50
n2 <- 50
n3 <- 50

colores <- matrix(NA,ncol=1,nrow=n)
colores[1:n1,1] <- "blue"
colores[(n1+1):(n1+n2),1] <- "green"
colores[(n1+n2+1):n,1] <- "orange"

pairs(X,pch=19,col=colores,main="Iris dataset") 

m.1 <- colMeans(X[Y[,5]=="setosa",])
m.1
m.2 <- colMeans(X[Y[,5]=="versicolor",])
m.2
m.3 <- colMeans(X[Y[,5]=="virginica",])
m.3



S <- (n - 1) / n * cov(X)
S

W <- (n1 - 1) * cov(X[Y[,5]=="setosa",])
W <- W + (n2 - 1) * cov(X[Y[,5]=="versicolor",])
W <- W + (n3 - 1) * cov(X[Y[,5]=="virginica",])
W

SW <- W / n
SW

lambdaa <- n * log(det(S)/det(SW))
lambdaa

# Percentil de la chi^2: chisq(1-nivel de signif, grados de libertad, lower.tail = F)
# Veamos con un nivel de significacion pequeño 0.001 ---> nivel de confianza alto 0.999
chisq=qchisq(0.999, 8, lower.tail = F)
chisq

# Vemos que lambda=563>>0.86=chisq, rechazamos H_0, rechazamos la hipótesis de que las medias de los grupos sean iguales.

#Vamos a usar ahora la otra definición de lambda que era para tamaños muestrales pequeños:
h <- (n - 1) - (p + G)/2
h

lambda0 <- h * log(det(S)/det(SW))
lambda0

# Vemos que lambda=546>>0.86=chisq, rechazamos H_0, rechazamos la hipótesis de que las medias de los grupos sean iguales.



