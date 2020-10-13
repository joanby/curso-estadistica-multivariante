
##################################################################################################################
##################################################################################################################
# Análisis de Clúster: Método de particiones: k-means
##################################################################################################################
##################################################################################################################

rm(list = ls()) 


X <- as.data.frame(state.x77)
X

X[,1] <- log(X[,1])
colnames(X)[1] <- "Log-Population"
X[,3] <- log(X[,3])
colnames(X)[3] <- "Log-Illiteracy"
X[,8] <- log(X[,8])
colnames(X)[8] <- "Log-Area"

##################################################################################################################
# k-means
##################################################################################################################

##################################################################################################################
# n, p: número de estados, número de variables
##################################################################################################################

dim(X)
n <- dim(X)[1]
n
p <- dim(X)[2]
p


# Estandarización univariante

X.s <- scale(X)


Kmeans.3 <- kmeans(X.s,3,nstart=25)

# SCDG

SCDG <- sum(Kmeans.3$withinss)
SCDG

# Clusters

Cl.kmeans <- Kmeans.3$cluster
Cl.kmeans

# Scatterplot matrix con la división en grupos resultante

col.cluster <- c("blue","red","green")[Cl.kmeans]
pairs(X.s,col=col.cluster,main="k-means",pch=19)

# Visualización con las primeras 2 componentes principales
install.packages("cluster")
library(cluster)
clusplot(X.s,Cl.kmeans)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette
dist.Euc <- dist(X.s,method="euclidean")
Sil.kmeans <- silhouette(Cl.kmeans,dist.Euc)
plot(Sil.kmeans,main="Silhouette for k-means",col="blue")






##################################################################################################################
##################################################################################################################
# Método de particiones: pam partition around medoids
##################################################################################################################
##################################################################################################################

pam.3 <- pam(X.s,3)

# Clusters

Cl.pam <- pam.3$clustering
Cl.pam

# Scatterplot matrix con la división en grupos resultante

col.cluster <- c("blue","red","green")[Cl.pam]
pairs(X.s,col=col.cluster,main="pam",pch=19)

# Visualización con las primeras 2 componentes principales

clusplot(X.s,Cl.pam)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

Sil.pam <- silhouette(Cl.pam,dist.Euc)
plot(Sil.pam,main="Silhouette for pam",col="blue")

