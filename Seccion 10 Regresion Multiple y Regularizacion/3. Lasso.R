install.packages(c("glmnet", "faraway"))
library(glmnet)   #lasso regression
library(faraway) #hdataset fat
set.seed(1233)
data("fat")
head(fat)

############################################################
#LASSO 
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

#Primero vamos a hallar la cantidad de penalización (lambda) por validación cruzada. 

#La función cv.glmnet() ajusta una regresión usando penalización,
#cuando le ponemos alpha=0 nos da el estimador rigde. El lambda elegido por cross-validation se encuentra
#en lambda.min:

#Tipo de penalización (alpha=1 lasso)
cv.lambda.lasso <- cv.glmnet(x=X, y=Y, 
                             alpha = 1) 
plot(cv.lambda.lasso) #MSE para varios lambdas (primera linea: lambda minimo, segunda linea: lambda + 1sd)

cv.lambda.lasso      #mejor lambda

#También podemos ver el impacto de diferentes lambdas en los 
#coeficientes estimados. Cuando es muy alto, los coeficientes se reducen a cero:

#Lasso path (penalización muy alta hace que los parámetros se encojan hacia cero)
plot(cv.lambda.lasso$glmnet.fit, 
     "lambda", label=FALSE)

#Con este lambda elegido podemos calcular los beta (parámetros de regresión)
l.lasso.min <- cv.lambda.lasso$lambda.min
lasso.model <- glmnet(x=X, y=Y,
                      alpha  = 1, 
                      lambda = l.lasso.min)
lasso.model$beta   



#¿Cuál es el porcentaje predicho de grasa (brozek) para alguien con estos datos?:
#age=24, weight=210.25, height=74.75, adipos=26.5, free=167.0, neck=39.0, chest=104.5, 
#abdom=94.4, hip=107.8, thigh=66.0, knee=42.0, ankle=25.6, biceps= 35.7, forearm=30.6, wrist=18.8

predict(lasso.model, 
        newx = matrix(c(age=24, weight=210.25, height=74.75, 
                        adipos=26.5, neck=39.0, chest=104.5, 
                        abdom=94.4, hip=107.8, thigh=66.0, 
                        knee=42.0, ankle=25.6,  biceps=35.7, 
                        forearm=30.6, wrist=18.8),
                      nrow = 1)
)





