library(MASS)

#Normal multivariante con media (0,0)
n = 100
mu1 = c(0,0)
sigma1 = matrix(c(1,0,0,1),2)
set.seed(1)
datos1 = mvrnorm(n,mu1,sigma1)

#Normal multivariante con media (5,5)
n = 100
mu2 = c(5,5)
sigma2 = matrix(c(1,0,0,1),2)
set.seed(2)
datos2 = mvrnorm(n,mu2,sigma2)

datos=rbind(datos1,datos2)

plot(datos,pch='*')

#Correlación para los 100 datos N((0,0),I)   ------> 0.0009943199
cor(datos1)[1,2]

#Correlación para los 100 datos N((5,5),I)   ------> 0.05546129
cor(datos2)[1,2]

#Correlación para los 200 datos              ------> 0.8614679
cor(datos)[1,2]
