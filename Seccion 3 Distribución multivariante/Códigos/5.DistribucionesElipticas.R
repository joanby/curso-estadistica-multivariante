library(MASS)
# Generar una muestra distribuida N(mu, Sigma) con correlacion alta y centrada en (2,2)
mu =  c(2,2)
Sigma = matrix(c(1, .8, .8, 1), 2) 
bivn <- mvrnorm(5000, mu , Sigma  ) # utiliza Mass package

#Plot de los datos bidimensionales
plot(bivn[,1],bivn[,2])

# Calcular densidades kernel 
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # MASS package

# Superficie 3-dimensional de la densidad 
# RGL interactive plot
library(rgl)
col1 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde, col = col1)

#Contornos
rho <- cor(bivn)
plot(bivn, xlab = "X", ylab = "Y",
     col = "cyan",
     main = "Bivariate Normal with ellipses")
lines(ellipse(mu,rho), col="red")       
lines(ellipse(mu,rho, alpha = .01), col="green")
lines(ellipse(mu,rho, alpha = .1), col="blue")
plot_legend <- c("99% CI green", "95% CI red","90% CI blue")
legend(4,0,legend=plot_legend,cex = 1, bty = "n")


####################################################################################
# Generar una muestra distribuida N(mu, Sigma) estandar, media (0,0) y cov=I
mu =  c(0,0)
Sigma = matrix(c(1, 0, 0, 1), 2) 
bivn <- mvrnorm(5000, mu , Sigma  ) # utiliza Mass package

#Plot de los datos bidimensionales
plot(bivn[,1],bivn[,2])

# Calcular densidades kernel 
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # MASS package

# Superficie 3-dimensional de la densidad 
# RGL interactive plot
library(rgl)
col1 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde, col = col1)

#Contornos
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) 
filled.contour(x = bivn.kde$x, y = bivn.kde$y , z = bivn.kde$z,plot.axes = { axis(1); axis(2); points(0, 0) })
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package
