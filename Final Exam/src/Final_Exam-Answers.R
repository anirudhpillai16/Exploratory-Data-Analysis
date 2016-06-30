
options(warn=-1)
#Question 1:

#The five R's of EDA are : Resistance, Residuals, Re-expression, Revelations and Reiteration.

#Question 2:
sample<- c(12,32,54,12,67,87,23,45,11,1) #sample of 10 numbers.
#There are two ways summary can be calculated:

#First way
fivenum(sample)


#Second way
summary(sample)

#There is a difference in values in using these two methods because quantiles are 
#calculated differently.


#Question 3:

#First, to linearize the data.

#Second, make the data normal.

#Third, Detection of outliers.

#Forth, To characterize uncertainity in estimates obtained from transformed data.

#Question 4:

#To detect long tailness we use qqplot or the histograms to detect if the data is normally distributed.
hist(sample)
qqplot(1:10,sample)
# we can use the kurtosis and skewness function defined in
# package e1071.
library(e1071)

kurtosis(sample)

skewness(sample)



#1) We can use Power transformation using Tukey's ladder of transformation, we can plot
#the points with respect to tukey's proposed value of x-axis and y-axis and then calculate
#the slope, subtracting it from 1 to get the value of power transformation.

#2) We can use the H distribution for long tails i.e. if h = 0 (Normal distribution, no tails)
#h>0 Long tails.
#To transform the data:
#X = A + B* Y_h(Z)
#Where, Y_h(Z) = Zh^((hZ^2)/2)

#Question 5:
#If g = 0, h=0 Gaussian Data , No skweness and No long tails.
#if g<0.25 , h>0 slight skewness with Long tail
#If g~1, h>0 Skewed with long tails.

#1. For, (-0.5,0.3) -> Left skewed with light tails.
#2. For, (0.5,0.3) -> Right skewed with light tails.
#3. For, (1,0.6) -> Heavily Right skewed with Heavy tails.

#Question 6:
#a)
a=c(1092,1137,1197,1237,1301,1523,1577, 1619,1626,1644,1672,1748,1768,1780, 1796,1816,1843,1844,1902,1919,1983, 1993,2025,2028,2032,2036,2072,2078, 2090,2137,2162,2163,2180,2185,2194,2225,2230,2233,2234,2235,2265,2270, 2274,2281,2289,2319,2322,2357,2381, 2398,2421,2421,2443,2522,2549,2552, 2581,2618,2618,2620,2624,2642,2647, 2666,2705,2721,2740,2804,2819,2823, 2860,2873,2906,2913,2926,2929,2931, 2931,2934,2939,2961,3020,3023,3044, 3047,3048,3096,3174,3190,3199,3204, 3222,3225,3278,3287,3292,3300,3339, 3361,3412,3462,3503,3530,3589,3672, 3734,3749,3783,3854,3901,3932,3995, 4001,4006,4118,4134,4320,4346,4385, 4401,4522,4565,4581,4593,4629,4855, 4868,4878,4885,4907,4962,4975,5021, 5127,5155,5160,5183,5229,5242,5379, 5383,5513,5555,5619,5755,5774,5890, 5899,5988,6161,6185,6818,7406,7419, 8175,8220,8282,8827,9027,9042,9805)

hist(a,probability = TRUE)
lines(density(a), col="red")
qqnorm(a)

#b)
source("lvalprogs.R")
lvals<- lval(a)
lvals

len <-length(a)
pp <- 1/2^(1:9); 
gau <- abs(qnorm(pp)) 
pp_2 <- (lvals[,1]-1/3)/(len + 1/3)
gau_2 <- abs(qnorm(pp_2)) 

est.g <- log((lvals[,3] - lvals[1,2])/(lvals[1,2]-lvals[,2]))/gau_2

plot(1:(dim(lvals)[1]-1), est.g[-1],
     xlab="Letter values",
     ylab="Estimate of g")
abline(h=median(est.g[-1]))
text(6.5,0.51,paste("median g = ",format(round(median(est.g[-1]),3))))

source("rrline.R")

# Estimate of A and B (selected p)
est.g <- median(est.g[-1]) 
p <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995)
zp <- qnorm(p)
est.Y <- (exp(est.g*zp)-1)/est.g
plot(est.Y,quantile(a,p),main="Estimate A and B",ylab=expression(x[p]),
     xlab=expression(paste((exp(gz[p])-1)/g,", g = 0.602")),
     sub="rrline estimates: A =2995.525, B =1318.138 ")
rr <- rrline1(est.Y,quantile(a,p))
abline(rr$a,rr$b)

#c)
library(boot)

#estimate of A 
data<-a
fboota <- function(d, i){
  d=d[i]
  fit=rrline1(est.Y,quantile(d,p))
  a=fit$a
  return(a) 
}

boot_corr_1<- boot(data, fboota, R = 1000)
boot.ci(boot_corr_1, type = "all",conf=0.9)

#Estimate of B
fbootb <- function(dat, i){
  dat=dat[i]
  fit=rrline1(est.Y,quantile(dat,p))
  b=fit$b
  return(b)
  }

boot_corr_2 <- boot(data,fbootb, R=1000)
boot.ci(boot.out = boot_corr_2, conf = 0.9, type = "all")

#Estimate of g

fbootg <- function(dat, i){
  n=length(data)
  dat_2=dat[i]
  lvals=lval(dat_2)
  pp <- (lvals[,1]-1/3)/(n + 1/3)
  gau <- qnorm(pp)
  est_1.g <- (-log((lvals[,3] - lvals[1,2])/(lvals[1,2]-lvals[,2]))/gau)
  est.g <- median(est_1.g[-1]) 
  return(est.g) 
}

boot_corr_3 <- boot(data,fbootg, R=1000)
boot.ci(boot_corr_3, type = "all",conf=0.9)

#Pairs plot
D=data.frame(as.vector(boot_corr_1$t), as.vector(boot_corr_2$t),as.vector(boot_corr_3$t))

A=D[,1]

B=D[,2]

g=D[,3]

pairs(~A+B+g,data=D)

#d)
g<-0.602
A<-2995.525
B<-1318.138
z<- 1/g*log(((a-A)*g)/B +1)
par(mfrow=c(1,2),mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
hist(z,prob=TRUE)
lines(density(z),col="blue")
qqnorm(z)


#e)
# Pearson Goodness of fit Test
gof.pearson=function (x,nbins) {
  n = length(x) 
  m = floor(n/nbins) 
  k = n - m*nbins
  xx=sort(x) 
  index = rep(1:nbins,m)
  if(k >0){ d=sample(1:nbins,k,replace=FALSE); 
  index=c(index,d) } 
  bincount=as.numeric(table(index)) 
  binindicies = cumsum(bincount) 
  binbreaks = rev(rev(xx[binindicies])[-1]) 
  binbreaks = c(-Inf,binbreaks,Inf) 
  bins=cut(x,breaks=binbreaks)
  internalbreaks = rev(rev(xx[binindicies])[-1]) 
  p = pnorm(internalbreaks,mean(x),sd(x))
  p = c(p[1],diff(p),1-pnorm(max(internalbreaks),mean(x),sd(x)))
  exp = n*p
  df = data.frame(bin=levels(bins),bincount=bincount,prob=p,expectedcount=exp)
  chisqstat = sum((bincount - exp)^2/exp)
  pval = 1- pchisq(chisqstat,nbins-1)
  output = list(df=df,chisq=chisqstat,pval=pval)
  output = list(df=df,chisq=chisqstat,pval=pval)
}
out=gof.pearson(z,2*sqrt(length(z))) 
out

# ECDF Based Test Statistics
library("goftest")
ks.test(z,"pnorm") #Kolmogorov Test

ad.test(z,"pnorm") #Anderson-Darling Test

cvm.test(z,"pnorm")#Cramer-von-Mises Test

#Correlation of the QQ Data test
qqnorm(z)
qqline(z)

#Shapiro Wilk's Test
shapiro.test(z)

#The above tests shows that distribution is normally distributed.

#Pearson's goodness of fit Test

#Pearson's chi-squared test uses a measure of goodness of fit which is the sum of differences between observed and expected outcome frequencies (that is, counts of observations), each squared and divided by the expectation
#The resulting value can be compared to the chi-squared distribution to determine the goodness of fit. In order to determine the degrees of freedom of the chi-squared distribution, one takes the total number of observed frequencies and subtracts the number of estimated parameters. The test statistic follows, approximately, a chi-square distribution with (k ??? c) degrees of freedom where k is the number of non-empty cells and c is the number of estimated parameters (including location and scale parameters and shape parameters) for the distribution.
# Sample with a large size is assumed.
# Observations are supposed to be independent.

#Shapiro Wilk's test:

#The null-hypothesis of this test is that the population is normally distributed. Thus if the p-value is less than the chosen alpha level, then the null hypothesis is rejected and there is evidence that the data tested are not from a normally distributed population. In other words, the data are not normal. On the contrary, if the p-value is greater than the chosen alpha level, then the null hypothesis that the data came from a normally distributed population cannot be rejected.

#QQ plot: QQplot is used to visualize the normality of the data. It is easy to compute.

#ECDF based statistics:

#Kolmogorov Test
#1. A feature of this test is that distribution of the K-S test statistics itself does not depend on underlying cumulative
#distribution function being tested.
#2. Another advantage is that it is an exact test.

#Limitations:

#1. It only applies to continuous distributions.
#2. It tends to be more sensitive near the center of distribution than at tails.
#3. Most serious limitation is that distribution must be fully specified.

#Anderson-Darling Test
#Many statistical tests and procedures are based on specific distributional assumptions. The assumption of normality is particularly common in classical statistical tests. Much reliability modeling is based on the assumption that the data follow a Weibull distribution.
#There are many non-parametric and robust techniques that do not make strong distributional assumptions. However, techniques based on specific distributional assumptions are in general more powerful than non-parametric and robust techniques. Therefore, if the distributional assumptions can be validated, they are generally preferred.


#Question 7:
b =c(12.87,15.09,17.39,18.62,20.24,23.76,24.35, 24.74,24.81,24.96,25.19,25.75,25.89,25.97, 26.07,26.19,26.35,26.36,26.67,26.76,27.07, 27.12,27.26,27.28,27.30,27.31,27.46,27.49, 27.54,27.72,27.81,27.82,27.88,27.90,27.93, 28.03,28.05,28.06,28.07,28.07,28.17,28.19, 28.20,28.22,28.25,28.34,28.35,28.46,28.53,28.58,28.64,28.65,28.70,28.92,28.99,29.00, 29.07,29.16,29.16,29.17,29.18,29.22,29.23, 29.28,29.37,29.40,29.45,29.59,29.62,29.63, 29.71,29.74,29.81,29.82,29.85,29.86,29.86, 29.86,29.87,29.88,29.92,30.04,30.05,30.09, 30.09,30.10,30.19,30.34,30.37,30.38,30.39, 30.43,30.43,30.53,30.55,30.55,30.57,30.64, 30.68,30.77,30.86,30.93,30.98,31.08,31.22, 31.32,31.35,31.41,31.52,31.60,31.65,31.76, 31.76,31.77,31.96,31.98,32.28,32.33,32.39, 32.42,32.61,32.68,32.71,32.73,32.79,33.15, 33.18,33.19,33.20,33.24,33.33,33.35,33.43, 33.60,33.65,33.66,33.70,33.77,33.80,34.03, 34.03,34.26,34.33,34.44,34.68,34.71,34.91, 34.93,35.09,35.40,35.44,36.63,37.81,37.84, 39.47,39.58,39.72,41.00,41.49,41.52,43.50)
par(mfrow=c(1,2),mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
hist(b,prob=TRUE)
lines(density(b),col="blue")
qqnorm(b)

lvals <- lval(b); 
#lvals
len<-length(b)
gh2.data <- b
lvals.gh2 <- lval(gh2.data)
yy.gh2 <- log(lvals.gh2[-1,6])
xx.gh2 <- (qnorm((lvals.gh2[-1,1] - 1/3)/(161 + 1/3)))^2/2 
plot(xx.gh2,yy.gh2,main="Estimate h and B", 
     ylab="log(pseudo-sigma)", xlab=expression(z[p]^2/2),
     sub="rrline: 2.71 + 0.24x => B = 2.71, h = 0.24")
rr <- rrline1(xx.gh2,yy.gh2);

abline(rr$a,rr$b)

exp(rr$a) # estimate B
rr$b # estimate h
median(b) #estimate A

#Estimate of B
data<-b
fbootb <- function(d, i){
  d=d[i]
  fit=rrline1(xx.gh2,quantile(d,p))
  a=fit$a
  return(a) 
}

boot_corr_1 <- boot(data,fbootb, R=1000)
boot.ci(boot_corr_1, type = "all",conf=0.95) #Confidence Interval for B

#Estimate of H
fbootg <- function(d, i){
  d=d[i]
  fit=rrline1(xx.gh2,quantile(d,p))
  g=fit$b
  return(g) 
}

boot_corr_2 <- boot(data,fbootg, R=1000)
boot.ci(boot_corr_2, type = "all",conf=0.95) #Confidence Interval for H

#Estimate of A
fboota <- function(d, i){
  d=d[i]
  return (median(d))}  

boot_corr_3 <- boot(data,fboota, R=1000) 
boot.ci(boot_corr_3, type = "all",conf=0.95) #Confidence Interval for A

# Pairs Plot
D=data.frame(as.vector(boot_corr_1$t), as.vector(boot_corr_2$t),as.vector(boot_corr_3$t))

A=D[,3]

B=D[,1]

h=D[,2]

pairs(~A+B+h,data=D)

#Question 8:

HDistBackXform=function(h,A,B,data){
  ###################################
  #  This function will allow you to back solve for Z
  #  under any H-distribution transform
  #  the Values h, A, and B are the estimated values of
  #  the H-distribution parameters.  In this program
  #  data is a vector of data.
  ###################################
  
  n=length(data)
  #using Veleman's rule
  output=numeric(n)
  g=function(z){z*exp(h*z^2)-((x-A)/B)}
  # Begin loop on i where data[i] is the ith data value
  for(i in 1:n){
    x=data[i]
    obj=uniroot(g,interval=c(-6,6))
    output[i]=obj$root
  }
  return(output)
}

h<-0.24
A<-29.92
B<-2.71

z<-HDistBackXform(h,A,B,b)
par(mfrow=c(1,2),mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
hist(z,prob=TRUE)
lines(density(z),col="red")
qqnorm(z)

noofbins=2*sqrt(length(z))#using Velema's rule
out<-gof.pearson(z,noofbins)
out

#Question 9:
q9data = rnorm(100, 3, 2)

getGausEstimate = function(data){
  d = density(data, kernel="gaussian")
  index = which(d$y == max(d$y), arr.ind =TRUE)
  ans = d$x[index]
  return(ans)
}

calculatePseudoValues = function(data) {
  n = length(data)
  yall = getGausEstimate(data)
  PV = numeric(n)
  for( i in 1:n) {
    yminusi = getGausEstimate(data[-i])
    PV[i] = n*yall - (n-1)*yminusi
  }
  return(PV)
}

# We first use jackknife 
PVAll = calculatePseudoValues(q9data)
n = length(PVAll)
# Jackknife estimate is 
mean(PVAll) 

jackKnifeEstimate = mean(PVAll)
varJK = sum((PVAll - jackKnifeEstimate)^2)/(n*(n-1))
seJK = sqrt(varJK)
seJK

getbootstrapestimate = function(data, nsim) {
  theta = numeric(nsim)
  varTheta = numeric(nsim)
  
  n = length(data)
  index = 1:n
  for (i in 1:nsim){
    sampleindex= sample(index,n,replace=TRUE)
    theta[i] = mean(getGausEstimate(data[sampleindex]))
  }
  
  output = list(thetaBS = mean(theta), varBS = var(theta),
                seBS = sqrt(var(theta)))
  output
}
# Now we calculate the Bootstrap estimate of the statistic
seBS = getbootstrapestimate(q9data, 100)$seBS 
seBS

#Question 10:
#To fit a robust resistant line we do the following:

#1.Sort the values and divide the observation say n into 3 equal sized groups.
#2.Find the summary(median x and median y) of the extreme groups.
#3.Calculate the slope using two point formula.
#4.Calculate the intercept.

#Advantages:
#1. Easy to calculate.
#2. The RRline is more robust and resistant towards outliers.
#3. Slope and intercept can easily be calculated.

#Disadvantages:
#1.It may take a lot time, depending on degree of resistance required.
#2.Unique solution is not guaranteed, depends on iterations.
#3.Bigger the dataset, more time the calculations take.

#Question 11:
#1. In bootstrapping we create the sample ,with replacements, of data from given data
#while in jackniffing we keep one data point at the back to test at the end when we do
#calculations.

#2.Bootstrapping is more relevant today than jacknifing.
 
#12)
#a)
observation_1 = c(5, 3, 0, 2, 0, 3, 2, 3, 6, 1, 2, 1, 2, 1, 3, 3, 3, 5, 2, 4)
observation_2 = c(4, 0, 2, 3, 7, 12, 3, 10, 9, 2, 3, 7, 7, 2, 3, 3, 6, 2, 4, 3)
observation_3 = c(5, 2, 2, 4, 0, 4, 2, 5, 2, 3, 3, 6, 5, 8, 3, 6, 6, 0, 5, 2)
observation_4 = c(2, 2, 6, 3, 4, 4, 2, 2, 4, 7, 5, 3, 3, 0, 2, 2, 2, 1, 3, 4)
observation_5 = c(2, 2, 1, 1, 1, 2, 1, 4, 4, 3, 2, 1, 4, 1, 1, 1, 0, 0, 2, 0)

observations <- c(observation_1,observation_2,observation_3,observation_4,observation_5)

years = c(1860:1959)

observation_year = cbind(observations,years)

combinedObservation = observation_year
Observations_dataSet = data.frame(combinedObservation)
colnames(Observations_dataSet) <- c("Inventions","Year")
uniqueOrderedDataFrame<-unique(Observations_dataSet)
count_table_data <- xtabs(~Inventions, data=uniqueOrderedDataFrame)
#Looking at the table it is a discrete frequency distributions. It may belong to poisson family or binomial family.For this we will try plotting poissonness plot, if we
#can fit a straight line then we can say that a poisson distribution is a good fit. If not we have to resort to plotting binomial distribution plot.
barplot(count_table_data)
count_table_data
x<-as.vector(count_table_data)
qqnorm(x)
qqline(x)

#Looking at qqnorm plot the distribution appears to be right skewed distribution.
hist(rbind(0:11,x),probability = TRUE, main = "Histogram of Density vs #Inventions")

#Just making some observations on my own below.

#install.packages("e1071")
library(e1071)

# skewness and kurtosis, they should be around (0,3)
skewness(count_table_data)
kurtosis(x)

########

#b)

#install.packages("vcd")
library(vcd)
#Just a function that I found on internet, pretty good! Th slope and intercept differs a bit, but we can get a direct estimate of lambda as well.
distplot(count_table_data)
#Slope is 1.412, intercept is -3.94, lambda = 3.1, exp(slope) = 4.105
#We then call y-axis a count/disribution metameter (by analogy with the use, in bioassay, of "response metameter" and "dose metameter"). The slope of such a theoretical line
#identifies the main parameter of the theoretical distribution.

#Code which we learned in class, poisoness plot along with Freeman-Tukey residuals.
source("poisplot.R")
poisplot(as.integer(names(count_table_data)),x)
#If the frequencies are Poisson distributed, then the Freeman-Tukey residuals are approximately normal distributed.
#When an isolated point strays from an apparently linear pattern of a
#Poissonness plot, we may want to judge more formally whether it is unlikely
#to have done so by chance. 
#Potting the residuals against k shows that the fit is quite good, except for the isolated count at k = 0 which does not follow the poisson distribution.
#######

#c)

#For an observed frequency ni and the estimated frequency mi, the Freeman-Tukey residual FTi is defined as FTi = sqrt(ni) + sqrt(ni + 1) - sqrt(4mi + 1).
#Or Freeman-Tukey residuals = sqrt(4* observed value of k + 2) - sqrt(4* expected value of k +1)
#Freeman and Tukey suggest this for a variance stabalizing transformation for Poisson data that leads to using the quantities defined above as residules.
#For a Poisson random variable X with mean m, Freeman and Tukey (1949) point out  that the expected
#value of sqrt(X) + sqrt(X+1) is well approximated by sqrt(4(n) + 1), and its variance
#is close to 1. Substituting n for fitted value leads to the residual.
#sqrt(x) + sqrt(x+1) - sqrt(4 * fitted value + 1)
#whose behavior is approximately that of an observation from the standard Gaussian distribution. 

#All values except for k=0 are reasonable values, they follow poisson distribution because they are within the red-line in FT residual plot.

######


#13)
row1<-c(16.0, 13.6, 16.2, 14.2,  9.3, 15.1, 10.6, 12.0, 11.3, 10.5,  7.7, 10.6)
row2<-c(30.4, 27.3, 32.4, 24.1, 27.3, 21.0, 19.2, 22.0, 19.4, 14.9, 11.4, 18.0)
row3<-c(34.8, 37.1, 40.3, 30.3, 35.0, 38.1, 26.2, 30.6, 25.8, 18.1, 12.3, 17.9)
row4<-c(37.2, 41.8, 42.1, 34.6, 38.8, 34.0, 30.0, 31.8, 27.9, 18.9, 13.0, 17.9)
row5<-c(35.3, 40.6, 42.9, 32.5, 38.6, 38.9, 30.9, 32.4, 28.5, 19.5, 12.5, 17.9)
row6<-c(39.2, 41.4, 43.9, 35.4, 37.5, 39.6, 32.4, 31.1, 28.1, 22.2, 13.7, 18.9)
row7<-c(39.7, 44.3, 45.5, 38.7, 42.4, 41.4, 35.5, 31.5, 27.8, 21.9, 14.4, 19.9)
rowNames<-c(95,175,250,350,500,675,1000)
colNames<-c(0111, 0211, 0311, 0412, 0512, 0612, 0721, 0821, 0921, 1022, 1122, 1222)
CO2PlantTable <- rbind(row1,row2,row3,row4,row5,row6,row7)
rownames(CO2PlantTable)<-rowNames
colnames(CO2PlantTable)<-colNames


#a)

medPolished<-medpolish(CO2PlantTable)
medPolished

#b)

AnalogRSqr<- 1-((sum(abs(medPolished$residuals))) /(sum(abs(CO2PlantTable-medPolished$overall))))
AnalogRSqr

#c)

#The diagnostic plot is a transformation plot for the two way table. Let y_ij be the response for row i and column j of a 
#a two way table. Decompose the data according to y_ij = m + a_i + b_j +r_ij where m, a_i, and b_j are 
#resistantly determined estimates for the common value, row effects, and column effects, respectively. The diagnostic plot
#has the comparision values, (a_i)(b_j)/m on its horizontal axis and the residuals from the additive fit,
# r_ij = y_ij -(m + a_i + b_j) on its vertical axis. When the pattern is roughly linear, 1-slope is the power transformation
#for the y_ij to promote additive structure.

x<- vector()
y<- vector()
for(i in 1:length(medPolished$row)){
  for(j in 1:length(medPolished$col)){
    x<- c(x,(medPolished$row[i] * medPolished$col[j])/medPolished$overall)
  }
  
}


residuals<-vector()
for (i in 1:7){
  residuals<-c(residuals,medPolished$residuals[i,])
}
plot(x,residuals,xlab="Comparison Values",ylab="Residual Values",main="Diagnostic plot")
abline(h=0,v=0)
fit<-lm(residuals~x)
abline(fit)
slope = fit$coefficients[[2]]
p = 1- slope
p #is the power transform


#d) Yes, transformation is needed.

#After transformations


CO2PlantTable.transform<-(CO2PlantTable)^(p)
CO2PlantTable.transform<-matrix(CO2PlantTable.transform,c(7,12))
dimnames(CO2PlantTable.transform)=list(rowNames,colNames) 
CO2PlantTable.transform.MP <- medpolish(CO2PlantTable.transform)

CO2PlantTable.transform.MP

MedianPolishdata<-rbind(CO2PlantTable.transform,CO2PlantTable.transform.MP$col)
MedianPolishdata<-cbind(MedianPolishdata,CO2PlantTable.transform.MP$row)
colnames(MedianPolishdata)[13]<-"Row Effect"
rownames(MedianPolishdata)[8]<-"Column Effect"
MedianPolishdata[8,13]<-medPolished$overall

#After transformation
MedianPolishdata 

sum_res<-sum(abs(CO2PlantTable.transform.MP$residual))
sum_data<-sum(abs(CO2PlantTable.transform-CO2PlantTable.transform.MP$overall))

Analogrsquare<-1-(sum_res/sum_data)
Analogrsquare  #Analog R square after Transformation.


#e)
library(aplpack)
stem.leaf(CO2PlantTable.transform.MP$residuals, m=2)

#Yes, there are outliers.

#f)
#Boxplot along rows
boxplot(t(CO2PlantTable.transform.MP$residuals))
#Boxplot along columns 
boxplot(CO2PlantTable.transform.MP$residuals)

#g)

source("myplotfit.r")
myplotfit(CO2PlantTable.transform.MP)

#(i)  Plant Combination has the largest effect than CO2 level.

#(ii) The highest combination of influence is of (95,1122) with a value of 1.

#h)
