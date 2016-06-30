#1 a)
library(noncensus)
data(counties)
library(car)

completeCounties<-counties[complete.cases(counties),]
stateAggre<-with(counties, aggregate(counties$state, list(counties$state), FUN=unique))
med<-with(counties, aggregate(counties$population, list(counties$state),FUN=median))
median<-as.vector(med$x)
state<-as.matrix(stateAggre$x)
stateMedianCombine <- cbind(median, state)
completeMedian<-stateMedianCombine[complete.cases(stateMedianCombine),]

quantiles<-with(counties, aggregate(counties$population, list(counties$state),FUN=quantile, na.rm = TRUE ))

completeQuantiles <- quantiles[complete.cases(quantiles),]
Forths<-matrix()
for(i in 1:nrow(quantiles$x)){
  Forths<-c(Forths,(quantiles$x[i,'75%'] - quantiles$x[i,'25%']))
}
transForths<- t(t(Forths))
completeForths <- transForths[complete.cases(transForths)]
matrixMH<- cbind(completeMedian,completeForths)
mX<-as.numeric(matrixMH[-c(8),1])
mY<-as.numeric(matrixMH[-c(8),3])
matrixMH2 <- cbind(mX,mY)

#Level vs Spread Plot
spreadLevelPlot(matrixMH2,by=matrixMH[-c(8),2],main="Spread Vs Level Plot")

#Scatter plot with log transform
#scatterplotMatrix(log(matrixMH))
#abline(lm(log(matrixMH[-c(7),1])~log(matrixMH[-c(7),2])))

#matrixMHState<-cbind(matrixMH,as.matrix(stateAggre$x))


plot(log(mX)~log(mY))
abline(mod<-lm(log(mX)~log(mY)))
slope<- coef(mod)[2]
slope

# Equation of line that fits is y=0.93x+c where Slope m=0.93
# Substituting (10,9.5) we get c=0.2
# Equation of line is y=0.93x+0.2
# b)
#The slope of the line is is 0.93, so p = 1-b = 1-0.93 = 0.07 = 0 (approx)
#T(x) = log(x)
boxplot(log(matrixMH2),main="Box Plot")
# c)
# Without Transform
boxplot(completeCounties$population~completeCounties$state,main="Box Plot
        without Transform")
# After Transform
boxplot((log(completeCounties$population)~completeCounties$state),
main="Box Plot after Transform")

# d)
source("C:/Users/lenovo/Documents/lvalprogs.R")
CAsubset<- completeCounties[completeCounties["state"] == "CA",]
letterValues <- lval(CAsubset$population)
#root<-letterValues^(1/30)
VectorXL <- as.vector(letterValues[,2])
VectorXU <- as.vector(letterValues[,3])

M <- letterValues[1,"Lower"] 

Y<- (VectorXL + VectorXU)/2 - M
X<- ((VectorXL-M)^2 + (VectorXU - M)^2) / (4*M)
letterValue<-c("M","F","E","D","C","B");
p<- (1-Y/X)
p
Table<- data.frame(letterValue,VectorXL,VectorXU,X,Y,p)

# e)
plot(Y~X)
abline(mod<-lm(Y~X))
slope <- coef(mod)[2]
slope
# b=0 after rounding slope to nearest 0.5
# Power transform p= 1-0= 1.

# f)
# T(x) = log(x), then T'(x) = 1/x i.e. z = x0*(1 + log(x) - log(x0)) = 427761.5 * log(x) - 5118731
#a = 427761.5, b = - 5118731