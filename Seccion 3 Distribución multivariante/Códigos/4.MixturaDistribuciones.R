
library(mixtools)
library(MASS)
library(tidyverse)
library(ggplot2)
library(rgl)

#Generando datos bidimensionales Normales con media (0,0) y S=I_2
## x ~ N_2(0, diag(2)) 
bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2)) 
plot(bivn)
#Generando datos bidimensionales Normales con diferente media y cov
## x ~ N_2(mu, diag(2)) 
bivn2 <- mvrnorm(800, mu = c(2,3), Sigma = matrix(c(1, 0.7, 0.7, 1), 2)) 
plot(bivn2)
#Mixtura
mix <- rbind(bivn,bivn2)
plot(mix)

# Calcular densidades kernel 
mix.kde <- kde2d(mix[,1], mix[,2], n = 50)   # MASS package
# Superficie 3-dimensional de la densidad 
# RGL interactive plot

col1 <- heat.colors(length(mix.kde$z))[rank(mix.kde$z)]
persp3d(mix.kde, col = col1)

#Contornos
filled.contour(x = mix.kde$x, y = mix.kde$y , z = mix.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(mix.kde)       # from base graphics package
contour(mix.kde, add = TRUE)     # from base graphics package
#Contornos
a <- data.frame( a1=mix[,1], a2=mix[,2] )
# Caso 1
# Grafico de contornos
# Solo contorno
ggplot(a, aes(x=a1, y=a2) ) +
  geom_density_2d()
# Solo area
ggplot(a, aes(x=a1, y=a2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Area + contorno
ggplot(a, aes(x=a1, y=a2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")


###################################################################################################
#Una mixtura menos compleja
#Generando datos bidimensionales Normales con media (0,0) y S=I_2
## x ~ N_2(0, diag(2)) 
bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), 2)) 
plot(bivn)
#Generando datos bidimensionales Normales con media mu=(5,5) y S=I_2
## x ~ N_2(mu, diag(2)) 
bivn2 <- mvrnorm(1000, mu = c(5, 5), Sigma = matrix(c(1, 0, 0, 1), 2)) 
plot(bivn2)
#Mixtura
mix <- rbind(bivn,bivn2)
plot(mix)

# Calcular densidades kernel 
mix.kde <- kde2d(mix[,1], mix[,2], n = 50)   # MASS package
# Superficie 3-dimensional de la densidad 
# RGL interactive plot
col1 <- heat.colors(length(mix.kde$z))[rank(mix.kde$z)]
persp3d(mix.kde, col = col1)

#Contornos
filled.contour(x = mix.kde$x, y = mix.kde$y , z = mix.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(mix.kde)       # from base graphics package
contour(mix.kde, add = TRUE)     # from base graphics package


#Contornos
a <- data.frame( a1=mix[,1], a2=mix[,2] )
# Caso 1
# Grafico de contornos
# Solo contorno
ggplot(a, aes(x=a1, y=a2) ) +
  geom_density_2d()
# Solo area
ggplot(a, aes(x=a1, y=a2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Area + contorno
ggplot(a, aes(x=a1, y=a2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

