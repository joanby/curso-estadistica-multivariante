##################################################################################################################
##################################################################################################################
# Análisis de Clúster: Métodos jerárquicos
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


dim(X)
n <- dim(X)[1]
n
p <- dim(X)[2]
p

##################################################################################################################
##################################################################################################################
# Método jerárquico aglomerativo
##################################################################################################################
##################################################################################################################

# Cargamos la librería cluster

library(cluster)

# Estandarizamos los datos

X.s <- scale(X)

##################################################################################################################
# Single linkage
##################################################################################################################

Agnes.Euc.Single <- agnes(X.s,metric="euclidean",method="single")

# Dendograma

plot(Agnes.Euc.Single,main="Single linkage")

# Cuantos clusters?

rect.hclust(Agnes.Euc.Single,k=3,border="blue")

# Asignación de cada observacion a un cluster

Cl.Agnes.Euc.Single <- cutree(Agnes.Euc.Single,3)
Cl.Agnes.Euc.Single

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[Cl.Agnes.Euc.Single]
pairs(X.s,col=col.cluster,main="Single linkage",pch=19)

# 2 primeros componentes principales con la agrupación obtenida

clusplot(X.s,Cl.Agnes.Euc.Single)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

dist.Euc <- dist(X.s,method="euclidean")
Sil.Agnes.Euc.Single <- silhouette(Cl.Agnes.Euc.Single,dist.Euc)
plot(Sil.Agnes.Euc.Single,main="Silhouette for Agnes and Single",col="blue")

##################################################################################################################
# Complete linkage
##################################################################################################################

Agnes.Euc.Complete <- agnes(X.s,metric="euclidean",method="complete")

# Dendograma

plot(Agnes.Euc.Complete,main="Complete linkage")

# Cuantos clusters?

rect.hclust(Agnes.Euc.Complete,k=3,border="blue")

# Asignación de cada observacion a un cluster

Cl.Agnes.Euc.Complete <- cutree(Agnes.Euc.Complete,3)
Cl.Agnes.Euc.Complete

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[Cl.Agnes.Euc.Complete]
pairs(X.s,col=col.cluster,main="Complete linkage",pch=19)

# 2 primeros componentes principales con la agrupación obtenida

clusplot(X.s,Cl.Agnes.Euc.Complete)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

Sil.Agnes.Euc.Complete <- silhouette(Cl.Agnes.Euc.Complete,dist.Euc)
plot(Sil.Agnes.Euc.Complete,main="Silhouette for Agnes and Complete",col="blue")

##################################################################################################################
# Average linkage
##################################################################################################################

Agnes.Euc.Average <- agnes(X.s,metric="euclidean",method="average")

# Dendograma

plot(Agnes.Euc.Average,main="Average linkage")

# Cuantos clusters?

rect.hclust(Agnes.Euc.Average,k=3,border="blue")

# Asignación de cada observacion a un cluster

Cl.Agnes.Euc.Average <- cutree(Agnes.Euc.Average,3)
Cl.Agnes.Euc.Average

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[Cl.Agnes.Euc.Average]
pairs(X.s,col=col.cluster,main="Average linkage",pch=19)

# 2 primeros componentes principales con la agrupación obtenida

clusplot(X.s,Cl.Agnes.Euc.Average)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

Sil.Agnes.Euc.Average <- silhouette(Cl.Agnes.Euc.Average,dist.Euc)
plot(Sil.Agnes.Euc.Average,main="Silhouette for Agnes and Average",col="blue")

##################################################################################################################
# Ward linkage
##################################################################################################################

Agnes.Euc.Ward <- agnes(X.s,metric="euclidean",method="ward")

# Dendograma

plot(Agnes.Euc.Ward,main="Ward linkage")

# Cuantos clusters?

rect.hclust(Agnes.Euc.Ward,k=3,border="blue")

# Asignación de cada observacion a un cluster

Cl.Agnes.Euc.Ward <- cutree(Agnes.Euc.Ward,3)
Cl.Agnes.Euc.Ward

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[Cl.Agnes.Euc.Ward]
pairs(X.s,col=col.cluster,main="Ward linkage",pch=19)

# 2 primeros componentes principales con la agrupación obtenida

clusplot(X.s,Cl.Agnes.Euc.Ward)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

Sil.Agnes.Euc.Ward <- silhouette(Cl.Agnes.Euc.Ward,dist.Euc)
plot(Sil.Agnes.Euc.Ward,main="Silhouette for Agnes and Ward",col="blue")

##################################################################################################################
##################################################################################################################
#  Método jerárquico divisivo
##################################################################################################################
##################################################################################################################

Diana.Euc <- diana(X.s,metric="euclidean")

# Dendograma

plot(Diana.Euc,main="Diana")

# Cuantos clusters?

rect.hclust(Diana.Euc,k=3,border="blue")

# Asignación de cada observacion a un cluster

Cl.Diana.Euc <- cutree(Diana.Euc,3)
Cl.Diana.Euc

# Scatterplot matrix con la agrupación obtenida

col.cluster <- c("blue","red","green")[Cl.Diana.Euc]
pairs(X.s,col=col.cluster,main="Diana",pch=19)

# 2 primeros componentes principales con la agrupación obtenida

clusplot(X.s,Cl.Diana.Euc)
text(princomp(X.s)$scores[,1:2],labels=rownames(X.s),pos = 1,col="blue")

# Silhouette

Sil.Diana.Euc <- silhouette(Cl.Diana.Euc,dist.Euc)
plot(Sil.Diana.Euc,main="Silhouette for Diana",col="blue")
