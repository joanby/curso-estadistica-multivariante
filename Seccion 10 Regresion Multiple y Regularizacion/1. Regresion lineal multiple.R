install.packages("faraway")
library(faraway) #dataset fat
set.seed(1233)
data("fat")
head(fat)

############################################################
#OLS 
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

#Correlaciones
c=cor(X)
c

install.packages("corrplot")
library(corrplot)
corrplot(c, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#Scatterplot
pairs(fat[,8:12], pch = 19)

install.packages("psych")
library(psych)
pairs.panels(fat[,8:12], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


#Regresión OLS
ols.regression <- lm(brozek ~  age + weight +                 
                       height + adipos +
                       neck + chest + 
                       abdom + hip + thigh +
                       knee + ankle + 
                       biceps + forearm + 
                       wrist, data=fat)

#Resumen del modelo:
summary(ols.regression)









