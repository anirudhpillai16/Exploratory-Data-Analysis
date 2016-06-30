#To find value of y, substitute i
y<- vector()
Mu<- expression(t + 0.5 * (exp(-50*(t-0.5)^2)))
mu<- function(dp){
  return (dp + 0.5 * (exp(-50*(dp-0.5)^2)))
}
x<- seq(0,1,length.out=50)
f<-function(t){ (2 * t - 1)/100}
time<- f(x)
t<-time
error <- rnorm(50,0,0.5)
y<-mu(time)+error
#y[i] <- mu(time) + error[i]
dataFrame<- cbind(time,y)
plot(dataFrame)
plot(mu(time))
m2<-1
rk<-1/(2*sqrt(pi))
sigma2<-0.5^2
n<-50
ddut<-D(D(Mu , 't'),'t')
J2Mu <- function(t){( -(0.5 * (exp(-50 * ((t - 0.5)^2)) * (50 * 2) - exp(-50 * ((t - 
                                                                                   0.5)^2)) * (50 * (2 * (t - 0.5))) * (50 * (2 * (t - 0.5))))))^2}
IntegrateJ2Mu <- integrate(J2Mu,lower=0,upper = 1)
J2muValue <- IntegrateJ2Mu$value

lamda_optimum<-(n^(-1/5))*(((sigma2)*rk)/(J2muValue*(m2^2)))^1/5
plot(x,y,main="Kernel Estimation",xlab="Time")
lines(ksmooth(x,y,kernel="normal",bandwidth=0.084))