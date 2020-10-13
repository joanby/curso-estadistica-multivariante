
##################################################################################################################
##################################################################################################################
# Escalado multidimensional
##################################################################################################################
##################################################################################################################


##################################################################################################################
# Distancias entre ciudades de Europa
##################################################################################################################

# Cargar los datos

data.dist <- eurodist
data.dist 

# Convertirlos en matriz

data.dist <- as.matrix(data.dist)
data.dist

# ¿Cuantas ciudades hay?

n <- nrow(data.dist)
n

# La función "cmdscale" es para hacer escalado multidimensional clásico 

mds.cities <- cmdscale(data.dist,eig=TRUE)

# Dentro del objeto anterior podemos encontrar los autovalores "mds.cities$eig"
# Gráfico de autovalores 

plot(mds.cities$eig,pch=19,col="blue",xlab="Number",ylab="Eigenvalue",type="o")
abline(a=0,b=0,col="red")

# Hay autovalores negativos

# Consideramos la solución de r=2 coordenadas principales

# Medida de precisión

m <- sum(abs(mds.cities$eig[1:2])) / sum(abs(mds.cities$eig))
m

# Obtener las coordenadas principales fijando k=2 para que lo haga con los dos primeros autovalores 

mds.cities <- cmdscale(data.dist,eig=TRUE,k=2)

x1 <- mds.cities$points[,1]
x2 <- mds.cities$points[,2]

# Veamos un gráfico en dos dimensiones de los datos con las coordenas obtenidas
 
plot(x1,x2,pch=19,col="blue",xlim=range(x1)+c(0,600))
text(x1,x2,pos=4,labels=rownames(data.dist),col="red")

# Vamos a invertir el eje Y para hacer más fácil la comparación con un mapa
# Las posiciones de las ciudades son muy similares a las posiciones reales en un mapa
x2 <- -x2

plot(x1,x2,pch=19,col="blue",xlim=range(x1)+c(0,600))
text(x1,x2,pos=4,labels=rownames(data.dist),col="red")



##################################################################################################################
##################################################################################################################
# Escalado no métrico de los datos de votaciones
##################################################################################################################
##################################################################################################################

rm(list = ls()) # Borramos los resultados del ejemplo anterior

library(MASS) 
install.packages("HSAUR2")
library(HSAUR2)

data("voting",package="HSAUR2")

voting.mds <- isoMDS(voting) # Escalado no métrico

voting.mds$points

plot(voting.mds$points[,1],voting.mds$points[,2],main="Non-metric scaling",pch=19,col="blue",
     xlab="First coordinate",ylab="Second coordinate")
text(voting.mds$points,labels=rownames(voting.mds$points),pos = 1,col="red")



