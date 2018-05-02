# Projt MRR

# Lecture de la data
setwd("C:/Users/USER-PC/Desktop/ProjetMrr/MRR")
tab <- read.csv(file="parkinsons_updrs.data",header=TRUE, sep=",")
library("lars")
library("corrplot")
library("ridge")
library("MASS")
library("glmnet")
library("ggplot2")
library("caret")


# On normalise les données :
tab_scale=tab
tab_scale=scale(tab_scale, center=FALSE)
dataY=data[,-5]
data_scale=scale(dataY, center=TRUE)
tab_scale[,5]=tab[,5]
#la matrice de corr
mat_cor <- cor(scale(as.matrix(tab_scale)))
mat_cor

corrplot(mat_cor, type="upper", order="hclust", tl.col="black", tl.srt=0)


#affichage des résiduts 
tab_scale= data.frame(tab_scale)
modreg <- lm(motor_UPDRS~.,data=tab_scale)
summary(modreg)
plot(modreg$residuals)
#2
# Regression Backward pour selection des variables explicatives :

regbackward = step(modreg,direction='backward')
summary(regbackward)

# Selection Forward :
modreg2 <- lm(motor_UPDRS~1,data=tab_scale)

regforward = step(modreg2,list(upper=modreg),direction='forward')
summary(regforward)

AIC(regforward)
AIC(regbackward)
AIC(modreg)

# Selection Stepwise :

regboth=step(modreg,direction='both')
summary(regboth)
AIC(regboth)

# By default R comes with few datasets. 
data = tab_scale
dataY= data$motor_UPDRS

dim(data)  # 5875


#Sample Indexes
indexes = sample(1:nrow(data), size=0.3*nrow(data))

# Split data
test = data[indexes,]
dim(test)  # 1762
train = data[-indexes,]
dim(train) # 4113


Xtest<-test[,-c(5)]
x.test=as.matrix(Xtest)
Ytest=test$motor_UPDRS
y.test=as.matrix(Ytest)



Xtrain<-train[,-c(5)]
x.train=as.matrix(Xtrain)
Ytrain=train$motor_UPDRS
y.train=as.matrix(Ytrain)
nrow(y.train)
nrow(Xtrain)

# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}
#Plot solution path and cross-validated MSE as function of ??
# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

mse0
mse1 
mse2 
mse3
mse4 
mse5 
mse6
mse7 
mse8 
mse9 
mse10

data3=as.matrix(data)
b=predict(fit.lasso,s=fit.lasso$lambda.min,newx=data3)
plot(b,data$motor_UPDRS)
nrow(b)
b