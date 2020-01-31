##################################################################################################################
##################################################################################################################
# Clúster basado en modelos: M-clust
##################################################################################################################
##################################################################################################################
rm(list = ls()) 


# Vamos a usar los datos originales sin escalarlos

X <- as.data.frame(state.x77)
X

X[,1] <- log(X[,1])
colnames(X)[1] <- "Log-Population"
X[,3] <- log(X[,3])
colnames(X)[3] <- "Log-Illiteracy"
X[,8] <- log(X[,8])
colnames(X)[8] <- "Log-Area"

dim(X)
n <- dim(X)[1]
n
p <- dim(X)[2]
p


library(mclust)

mclust.X <- Mclust(X,modelNames = c("VEE"))
mclust.X

# Clusters

Cl.mclust <- mclust.X$classification
Cl.mclust

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[mclust.X$classification]
pairs(X,col=col.cluster,main="M-clust solution",pch=19)

# First two PCs with cluster solution 
X.s <- scale(X)
clusplot(X,Cl.mclust)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Aquí están los parámetros del modelo de mixtura

mclust.X$parameters




