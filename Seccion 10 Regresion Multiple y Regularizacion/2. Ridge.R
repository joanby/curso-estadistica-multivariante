install.packages(c("glmnet", "faraway"))
library(glmnet)   #ridge regression
library(faraway) #dataset fat
set.seed(1233)
data("fat")
head(fat)

############################################################
#RIDGE 
############################################################

#Definimos la ecuación del modelo
X <- model.matrix(brozek ~  age + weight + 
                    height + adipos +
                    neck + chest + 
                    abdom + hip + thigh +
                    knee + ankle + 
                    biceps + forearm + 
                    wrist, data=fat)[,-1]

#Y la variable dependiente
Y <- fat[,"brozek"] 

#Correlacion entre las X (la mayoría muy altas)
cor(X)


#Primero vamos a hallar la cantidad de penalización (lambda) por validación cruzada. 

#La función cv.glmnet() ajusta una regresión usando penalización,
#cuando le ponemos alpha=0 nos da el estimador rigde. El lambda elegido por cross-validation se encuentra
#en lambda.min:

#Tipo de penalización (alpha=0 ridge)
cv.lambda <- cv.glmnet(x=X, y=Y, 
                       alpha = 0) 

plot(cv.lambda) #MSE para varios lambdas (primera linea: lambda minimo, segunda linea: lambda + 1sd)

cv.lambda$lambda.min #mejor lambda

#También podemos ver el impacto de diferentes lambdas en los 
#coeficientes estimados. Cuando es muy alto, los coeficientes se reducen a cero:

#ridge path (penalización muy alta hace que los parámetros se encojan hacia cero)
plot(cv.lambda$glmnet.fit, 
     "lambda", label=FALSE)

#Con este lambda elegido podemos calcular los beta (parámetros de regresión)
lmin        <- cv.lambda$lambda.min
ridge.model <- glmnet(x=X, y=Y,
                      alpha = 0, 
                      lambda = lmin)

print(ridge.model)

ridge.model$beta




#Regresión OLS
ols.regression <- lm(brozek ~  age + weight +                 
                       height + adipos +
                       neck + chest + 
                       abdom + hip + thigh +
                       knee + ankle + 
                       biceps + forearm + 
                       wrist, data=fat)


#OLS vs RIDGE
round(cbind(OLS = coef(ols.regression), 
            ridge = c(ridge.model$a0,                              
                      as.vector(ridge.model$beta))),4)             








