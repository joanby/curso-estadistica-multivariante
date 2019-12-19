# Annotating a filled contour plot
install.packages("graphics")
install.packages("grDevices")
install.packages("MASS")
install.packages("mvtnorm")
install.packages("ellipse")
library(ellipse)
library(graphics)
library(grDevices)
library(MASS)
library(mvtnorm)

#################################### Example 1
x.points <- seq(-3,4,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x.points,y.points,z)


#################################### Example 2
x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(0,0)
sigma <- matrix(c(1,0,0,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x.points,y.points,z)




#################################### Example 3
bivn <- mvrnorm(5000, mu =  c(0,0), Sigma = matrix(c(1, 0, 0, 1), 2) )  # utiliza Mass package
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) 
filled.contour(x = bivn.kde$x, y = bivn.kde$y , z = bivn.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package



#################################### Example 4
mu =  c(0,0)
Sigma = matrix(c(1, 0, 0, 1), 2)
bivn4 <- mvrnorm(5000, mu , Sigma )  

rho <- cor(bivn4)
plot(bivn4, xlab = "X", ylab = "Y",
     col = "cyan",
     main = "Bivariate Normal with ellipses")
lines(ellipse(mu,rho), col="red")       
lines(ellipse(mu,rho, alpha = .01), col="green")
lines(ellipse(mu,rho, alpha = .1), col="blue")
plot_legend <- c("99% CI green", "95% CI red","90% CI blue")
legend(2.5,3.5,legend=plot_legend,cex = 1, bty = "n")


#################################### Example 5
# Library
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

#Generar datos Normales estandar:
bivn = mvrnorm(5000, mu =  c(0,0), Sigma = matrix(c(1, 0, 0, 1), 2) )
a <- data.frame( x=bivn[,1], y=bivn[,2] )
#Generar datos Normales correlacionados:
bivn2 = mvrnorm(5000, mu =  c(0,0), Sigma = matrix(c(1, 0.7, 0.7, 1), 2) )
b <- data.frame( x=bivn2[,1], y=bivn2[,2] )
# Caso 1
# Grafico de contornos
# Solo contorno
ggplot(a, aes(x=x, y=y) ) +
  geom_density_2d()
# Solo area
ggplot(a, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Area + contorno
ggplot(a, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

# Caso 2
# Grafico de contornos
# Solo contorno
ggplot(b, aes(x=x, y=y) ) +
  geom_density_2d()
# Solo area
ggplot(b, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Area + contorno
ggplot(b, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")


