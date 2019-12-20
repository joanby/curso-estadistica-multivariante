install.packages("mixtools")
library(mixtools)

#Generando datos bidimensionales t-student con 3 grados de libertad
## x ~ t_3(0, diag(2)) 
bivn <- rmvt(1000, sigma = diag(2), df = 3) 
plot(bivn)
# Media y matriz de cov
clcenter <- colMeans(bivn)
clcov <- cov(bivn)
# 97.5%, 75%, 50% elipses de tolerancia
ellipse(mu = clcenter, sigma = clcov, alpha = 0.025,col = "blue", lty = 2)
ellipse(mu = clcenter, sigma = clcov, alpha = 0.25,col = "green", lty = 3)
ellipse(mu = clcenter, sigma = clcov, alpha = 0.5,col = "red", lty = 3)

# Calcular densidades kernel 
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # MASS package

# Superficie 3-dimensional de la densidad 
# RGL interactive plot
library(rgl)
col1 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(bivn.kde, col = col1)

#Contornos
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) 
filled.contour(x = bivn.kde$x, y = bivn.kde$y , z = bivn.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

a <- data.frame( a1=bivn[,1], a2=bivn[,2] )
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




############################################################################################################
#Generando datos bidimensionales t-student con 10 grados de libertad
## x ~ t_10(0, diag(2)) 
bivn2 <- rmvt(1000, sigma = diag(2), df = 10) 
plot(bivn2)
# Media y matriz de cov
clcenter <- colMeans(bivn2)
clcov <- cov(bivn2)
# 97.5%, 75%, 50% elipses de tolerancia
ellipse(mu = clcenter, sigma = clcov, alpha = 0.025,col = "blue", lty = 2)
ellipse(mu = clcenter, sigma = clcov, alpha = 0.25,col = "green", lty = 3)
ellipse(mu = clcenter, sigma = clcov, alpha = 0.5,col = "red", lty = 3)


# Calcular densidades kernel 
bivn2.kde <- kde2d(bivn2[,1], bivn2[,2], n = 50)   # MASS package

# Superficie 3-dimensional de la densidad 
# RGL interactive plot
library(rgl)
col1 <- heat.colors(length(bivn2.kde$z))[rank(bivn2.kde$z)]
persp3d(bivn2.kde, col = col1)

#Contornos
bivn2.kde <- kde2d(bivn2[,1], bivn2[,2], n = 50) 
filled.contour(x = bivn2.kde$x, y = bivn2.kde$y , z = bivn2.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(bivn2.kde)       # from base graphics package
contour(bivn2.kde, add = TRUE)     # from base graphics package

b <- data.frame( b1=bivn2[,1], b2=bivn2[,2] )
# Caso 2
# Grafico de contornos
# Solo contorno
ggplot(b, aes(x=b1, y=b2) ) +
  geom_density_2d()
# Solo area
ggplot(b, aes(x=b1, y=b2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
# Area + contorno
ggplot(b, aes(x=b1, y=b2) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")



