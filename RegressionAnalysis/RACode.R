# set the path in R
setwd("C:../RegressionAnalysis") 
# Data Insertion
rm(list=ls())
Data<-read.table(file="data.txt" ,header=T ,na.strings=c("uns")) 

# Delete the observations with NA
Data <- na.omit(Data)

# Choose 38 observations
Data <- Data[sample(1:nrow(Data), size = 38), ]
Data

# Correlation Coefficient between Damage-Attitude
Damage<-Data[,1]
Attitude<-Data[,5]
cor.test(Damage,Attitude)



# Model Fit
results <-lm(Data$damage~Data$attitude)
summary(results)
confint(results)

#simple linear model hypothesis tests
plot(Data$attitude,Data$damage,xlab="Attitude",ylab="Damage") 
abline(results)
par(mfrow=c(1,2))
hist(results$res,main = "Histogram of residuals",xlab = "Residuals")
qqnorm(residuals(results)) 
qqline(residuals(results))
shapiro.test(results$residuals)
plot(results$res, results$fitted, xlab="Residuals",ylab="Expected Damage")
plot(results$res, Data$attitude , xlab="Residuals", ylab="Attitude")
plot(1:38,residuals(results), xlab="# of Observations", ylab="Residuals")


# Multiple Linear Model
Damage <- Data$damage
Sex <- as.factor(Data$sex)
Faction <- as.factor(Data$faction)
Class<-as.factor(Data$class)
Attitude <- Data$attitude
resultsMult <- lm(Damage~Attitude+Sex+Faction+Class)
resultsMult
confint(resultsMult)

summary(resultsMult)
confint(resultsMult)

# prediction
predict(resultsMult,list(Attitude=c(mean(Attitude)), Sex=c("woman"), Faction=c("elf"), Class=c("wizard")), int="c")