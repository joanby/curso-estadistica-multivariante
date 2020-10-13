install.packages(c("glmnet", "faraway"))
library(glmnet)   
library(faraway) 
install.packages("caret")
library(caret)
set.seed(1233)
data("fat")
head(fat)

############################################################
#Elastic Net 
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


#Intervalo para alpha y lambda:
search.grid <-expand.grid(alpha = seq(0,1,.1),
                          lambda = exp(seq(-1,1,.1)))

#Validacion cruzada
train.control <- trainControl(method = "cv", 
                              number = 10)
#Modelo
step.model <- train(brozek ~  age +
                        weight + 
                        height + adipos +
                        neck + chest + 
                        abdom + hip + thigh +
                        knee + ankle + 
                        biceps + forearm + 
                        wrist, 
                      data = fat,
                      method = "glmnet", 
                      trControl = train.control,
                      tuneGrid = search.grid)

#Alpha y lambda optimos:
step.model$bestTune$alpha
step.model$bestTune$lambda

#Coeficientes 
coef(step.model$finalModel, step.model$bestTune$lambda)


