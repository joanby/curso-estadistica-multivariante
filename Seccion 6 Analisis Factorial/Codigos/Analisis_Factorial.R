
##################################################################################################################
##################################################################################################################
# Análisis Factorial
##################################################################################################################
##################################################################################################################

rm(list = ls()) 

##################################################################################################################
# Vamos a cargar los datos state.x77
##################################################################################################################

X <- as.data.frame(state.x77)
X

##################################################################################################################
# Redefinimos el nombre de las variables con espacios
##################################################################################################################

colnames(X)[4] = "Life.Exp"
colnames(X)[6] = "HS.Grad"

##################################################################################################################
# n, p: numero de estados, numero de variables
##################################################################################################################

dim(X)
n <- dim(X)[1]
n
p <- dim(X)[2]
p

##################################################################################################################
# Scatterplot de las variables originales
##################################################################################################################

pairs(X,col="blue",pch=19,main="Original dataset")

##################################################################################################################
# Como las vairables tienen diferentes unidades de medida,
# vamos a usar la matriz de correlaciones para estimar la matriz de carga.
##################################################################################################################

mu <- colMeans(X)
mu
R <- cor(X)
R

##################################################################################################################
# Principal components factor analysis (PCFA)
##################################################################################################################

eR <- eigen(R)
eR

eigen.val <- eR$values # Auto-valores 
eigen.val
eigen.vec <- eR$vectors # Auto-vectores
eigen.vec
prop.var <- eigen.val / sum(eigen.val) # Proporcion de variabilidad
prop.var
prop.var.accum <- cumsum(eigen.val) / sum(eigen.val) # Proporcion de variabilidad acumulada
prop.var.accum

##################################################################################################################
# Ahora vamos a estimar la matriz de carga usando los auto-valores y auto-vectores. 
# Aplicaremos también el método varimax
##################################################################################################################

L.est.1 <- eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.1

L.est.1.var <- varimax(L.est.1)
L.est.1.var

##################################################################################################################
# Estimacion de la matriz de covarianza de los errores.
##################################################################################################################

Psi.est.1 <- diag(diag(R - as.matrix(L.est.1.var$loadings) %*% t(as.matrix(L.est.1.var$loadings))))
Psi.est.1

##################################################################################################################
# Ahora, podemos usar esta estimación para estimar mediante el método Principal factor analysis (PFA)
##################################################################################################################

RP <- R - Psi.est.1

eRP <- eigen(RP)
eRP

eigen.val <- eRP$values # Auto-valores 
eigen.val
eigen.vec <- eRP$vectors # Auto-vectores
eigen.vec
prop.var <- eigen.val / sum(eigen.val) # Proporcion de variabilidad
prop.var
prop.var.accum <- cumsum(eigen.val) / sum(eigen.val) # Proporcion de variabilidad acumulada
prop.var.accum

##################################################################################################################
# Ahora, podemos estimar la matriz de carga de nuevo
# Aplicaremos también el método varimax
##################################################################################################################

L.est.2 <- eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.2

L.est.2.var <- varimax(L.est.2)
L.est.2.var

##################################################################################################################
# Estimacion de la matriz de covarianza de los errores.
##################################################################################################################

Psi.est.2 <- diag(diag(R - as.matrix(L.est.2.var$loadings) %*% t(as.matrix(L.est.2.var$loadings))))
Psi.est.2

##################################################################################################################
# Obtengamos los "scores" para ambos métodos
##################################################################################################################
# PCFA
FS.est.1 <- scale(X) %*% as.matrix(L.est.1.var$loadings)
FS.est.1

# PFA
FS.est.2 <- scale(X) %*% as.matrix(L.est.2.var$loadings)
FS.est.2

##################################################################################################################
# Grafiquemos los scores
##################################################################################################################

par(mfrow=c(2,1))

plot(FS.est.1[,1],FS.est.1[,2],xlab="First factor",ylab="Second factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,1],FS.est.1[,2],labels=rownames(X),pos = 4,col="blue")

plot(FS.est.2[,1],-FS.est.2[,2],xlab="First factor",ylab="Second factor",main="Scores with second method",pch=19,col="blue")
text(FS.est.2[,1],-FS.est.2[,2],labels=rownames(X),pos = 4,col="blue")

plot(FS.est.1[,1],FS.est.1[,3],xlab="First factor",ylab="Third factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,1],FS.est.1[,3],labels=rownames(X),pos = 4,col="blue")

plot(FS.est.2[,1],FS.est.2[,3],xlab="First factor",ylab="Third factor",main="Scores with second method",pch=19,col="blue")
text(FS.est.2[,1],FS.est.2[,3],labels=rownames(X),pos = 4,col="blue")

plot(FS.est.1[,2],FS.est.1[,3],xlab="Second factor",ylab="Third factor",main="Scores with first method",pch=19,col="blue")
text(FS.est.1[,2],FS.est.1[,3],labels=rownames(X),pos = 4,col="blue")

plot(-FS.est.2[,2],FS.est.2[,3],xlab="Second factor",ylab="Third factor",main="Scores with second method",pch=19,col="blue")
text(-FS.est.2[,2],FS.est.2[,3],labels=rownames(X),pos = 4,col="blue")

##################################################################################################################
# Ahora, vamos a usar MLE para estimar la matriz de carga y los factores.
# Note que MLE siempre escala los datos.
# Por simplicidad vamos a usar el método de regresión para estimar los "scores".
##################################################################################################################

# Un factor

X.1 <- factanal(X, factors = 1, rotation="varimax",scores="regression")
X.1

# Dos factores

X.2 <- factanal(X, factors = 2, rotation="varimax",scores="regression")
X.2

# Tres factores

X.3 <- factanal(X, factors = 3, rotation="varimax",scores="regression")
X.3

# Cuatro factores

X.4 <- factanal(X, factors = 4, rotation="varimax",scores="regression")
X.4
##################################################################################################################
# Estimacion de la matriz de carga y la matriz de covarianza de los errores
##################################################################################################################

L.est.3.var <- X.4$loadings
L.est.3.var

Psi.est.3 <- diag(X.4$uniquenesses)
Psi.est.3

# Gráficos

par(mfrow=c(1,1))

plot(X.4$scores[,1],X.4$scores[,2],xlab="First factor",ylab="Second factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(1,2)],labels=rownames(X),pos = 4,col="blue")

plot(X.4$scores[,1],X.4$scores[,3],xlab="First factor",ylab="Third factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(1,3)],labels=rownames(X),pos = 4,col="blue")

plot(X.4$scores[,1],X.4$scores[,4],xlab="First factor",ylab="Fourth factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(1,4)],labels=rownames(X),pos = 4,col="blue")

plot(X.4$scores[,2],X.4$scores[,3],xlab="Second factor",ylab="Third factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(2,3)],labels=rownames(X),pos = 4,col="blue")

plot(X.4$scores[,2],X.4$scores[,4],xlab="Second factor",ylab="Fourth factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(2,4)],labels=rownames(X),pos = 4,col="blue")

plot(X.4$scores[,3],X.4$scores[,4],xlab="Third factor",ylab="Fourth factor",main="Scores with MLE",pch=19,col="blue")
text(X.4$scores[,c(3,4)],labels=rownames(X),pos = 4,col="blue")
