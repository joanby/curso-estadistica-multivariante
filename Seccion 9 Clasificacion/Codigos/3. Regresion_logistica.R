
##################################################################################################################
##################################################################################################################
# Regresión logística LR
##################################################################################################################
##################################################################################################################
##################################################################################################################
# Cargar librería MASS
##################################################################################################################

library(MASS)
set.seed(1000)
##################################################################################################################
# Cargar los datos Iris
##################################################################################################################

Z <- as.data.frame(iris)
Z

##################################################################################################################
# Definir la matriz de datos y la variable respuesta con las clasificaciones 
##################################################################################################################

X <- Z[,1:4]
X

y <- Z[,5]
y

##################################################################################################################
# Definir como n y p el número de flores y el número de variables
##################################################################################################################

n <- nrow(X)
n
p <- ncol(X)
p


# Cargar la librería nnet

library(nnet)

# Definir un vector para incluir las clasificaciones de cada observación usando leave-one-out cross-validation

lr.iris.class <- matrix(0,nrow=n,ncol=1)
lr.iris.probs <- matrix(0,nrow=n,ncol=3)

# Hacer la validación cruzada

for (i in 1:n){
  print(i)
  lr.iris <- multinom(y[-i] ~ .,data=X[-i,])
  lr.iris.probs[i,] <- predict(lr.iris,newdata=X[i,],type="probs")
  lr.iris.class[i] <- levels(y)[predict(lr.iris,newdata=X[i,])]
}

# Veamos las clasificaciones

lr.iris.class

# Tabla de contingencia

table.lr <- table(y,lr.iris.class)
table.lr

# Proporción de errores

mis.lr <- n - sum(y==lr.iris.class)
mis.lr/n

# Buenas clasificaciones en rojo, malas en negro

col.lr.iris <- c("black","indianred1")[1*(y==lr.iris.class)+1]
pairs(X,main="Good (in red) and bad (in black) classifications for the Iris data set with LR",pch=19,col=col.lr.iris)

# lr.iris.probs probabilidades de pertenencia a los grupos

lr.iris.probs

# Gráfico de probabilidades

plot(1:n,lr.iris.probs[,1],main="Posterior probabilities (blue, group 1, green, group 2 and orange, group 3)",pch=20,col="blue",
     xlab="Observation number",ylab="Probabilities")
points(1:n,lr.iris.probs[,2],pch=20,col="green")
points(1:n,lr.iris.probs[,3],pch=20,col="orange")

