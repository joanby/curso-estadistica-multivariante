# Datos de una Normal bivariante
n <- 500
x <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
                      sigma = rbind(c(1, 0), c(0, 1)))

# Hallar la kde 
kde <- ks::kde(x = x)

# eval.points contiene los grids de x e y
str(kde$eval.points)
## List of 2
##  $ : num [1:151] ...
##  $ : num [1:151] ...

# Los grids kde$eval.points se usan para calcular una matriz grid cuadrada kde$estimate 
# que es donde está la estimación kde.
dim(kde$estimate)
## [1] 151 151

# Plot
image(kde$eval.points[[1]], kde$eval.points[[2]], kde$estimate,
      col = viridis::viridis(20), xlab = "x", ylab = "y")
points(kde$x) # las observaciones


# Se pueden hacer cambios como por ejemplo el grid size y el tamaño del rectangulo donde se dibuja.
kde <- ks::kde(x = x, gridsize = c(500, 500), xmin = c(-4, -3),
               xmax = c(4, 3))
image(kde$eval.points[[1]], kde$eval.points[[2]], kde$estimate,
      col = viridis::viridis(20), xlab = "x", ylab = "y")
points(kde$x) 



# Plot de contornos
plot(kde, display = "slice", cont = c(25, 50, 75), xlab = "x", ylab = "y")
# "cont" especifica el porcentaje para los contornos, que delimitan las regiones de densidad. 

# Plot de contornos con puntos
plot(kde, display = "slice", cont = c(25, 50, 75), xlab = "x", ylab = "y")
points(kde$x)


# Contornos con relleno con la opción "col.fun"
plot(kde, display = "filled.contour2", cont = seq(5, 95, by = 10),
     xlab = "x", ylab = "y", col.fun = viridis::viridis)

# Añade los niveles (con add=TRUE se añade un grafico a otro)
plot(kde, display = "filled.contour", cont = seq(5, 95, by = 10),
     xlab = "x", ylab = "y", col.fun = viridis::viridis)
plot(kde, display = "slice", cont = seq(5, 95, by = 10), add = TRUE)



# Gráfico de perspectiva
plot(kde, display = "persp", col.fun = viridis::viridis, xlab = "x", ylab = "y")

# La estimación de densidad en R3 se puede visualizar a través de contornos 3D.

# Simulated data from a trivariate normal
n <- 500
x <- mvtnorm::rmvnorm(n = n, mean = c(0, 0, 0),
                      sigma = rbind(c(1.5, 0.25, 0.5),
                                    c(0.25, 0.75, 1),
                                    c(0.5, 1, 2)))

# Mostrar contornos anidados de regiones de alta densidad
plot(ks::kde(x = x, H = diag(c(rep(1.25, 3)))), drawpoints = TRUE, col.pt = 1)
rgl::rglwidget()







