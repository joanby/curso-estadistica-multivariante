
##################################################################################################################
##################################################################################################################
# Componentes Principales
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Cargamos el dataset state.x77
##################################################################################################################

X <- as.data.frame(state.x77)
X

##################################################################################################################
# Como podemos ver algunas de las variables tienen espacios en sus nombres, así que vamos a redefinir estos nombres.
##################################################################################################################

colnames(X)[4] = "Life.Exp"
colnames(X)[6] = "HS.Grad"

##################################################################################################################
# Definimos n y p el numero de estados y el numero de variables
##################################################################################################################

dim(X)
n <- dim(X)[1]
n
p <- dim(X)[2]
p

##################################################################################################################
# Graficamos las variables originales
##################################################################################################################

pairs(X,col="blue",pch=19,main="Original dataset")

##################################################################################################################
# Calcular la media muestral y la matriz de covarianza muestral
##################################################################################################################

mu <- colMeans(X)
mu
S <- cov(X)
S

##################################################################################################################
# Obtener los componentes principales en base a la matriz de covarianza muestral
##################################################################################################################

eS <- eigen(S)
eS

eigen.val <- eS$values # Auto-valores
eigen.val
eigen.vec <- eS$vectors # Auto-vectores
eigen.vec
prop.var <- eigen.val / sum(eigen.val) # Proporcion de variabilidad
prop.var
prop.var.accum <- cumsum(eigen.val) / sum(eigen.val) # Proporcion de variabilidad acumulada
prop.var.accum

##################################################################################################################
# Obtener los componentes principales en base a la matriz de correlaciones muestrales
##################################################################################################################

R <- cor(X)
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

mean(eigen.val) # media de los auto-valores

##################################################################################################################
# Obtener los scores
##################################################################################################################

ones <- matrix(rep(1,n),nrow=n,ncol=1)
X.cen <- as.matrix(X) - ones %*% mu
X.cen

Dx <- diag(diag(S))
Dx

Y <- X.cen %*% solve(Dx)^(1/2)
Y

scores <- Y %*% eigen.vec
colnames(scores) <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")
scores

##################################################################################################################
# Graficar los scores
##################################################################################################################

pairs(scores,main="Scores",col="blue",pch=19)

##################################################################################################################
# Screeplot
##################################################################################################################

screeplot(princomp(X,cor=T),main="Screeplot",col="blue",type="lines",pch=19)
#cor: a logical value indicating whether the calculation should use the correlation matrix or the covariance matrix.

##################################################################################################################
# Graficar los scores
##################################################################################################################

plot(scores[,1:2],xlab="First principal component",ylab="Second principal component",col="blue",pch=19,
     main="First and second principal component")
text(scores[,1:2],labels=rownames(X),pos = 1,col="blue")

plot(scores[,c(1,3)],xlab="First principal component",ylab="Third principal component",col="blue",pch=19,
     main="First and third principal component")
text(scores[,c(1,3)],labels=rownames(X),pos = 1,col="blue")

plot(scores[,2:3],xlab="Second principal component",ylab="Third principal component",col="blue",pch=19,
     main="Second and third principal component")
text(scores[,2:3],labels=rownames(X),pos = 1,col="blue")

##################################################################################################################
# Correlacion entre los componentes principales y la variables originales 
##################################################################################################################

Corr <- diag(eigen.val[1:3]^(1/2)) %*%  t(eigen.vec[,1:3])
Corr
