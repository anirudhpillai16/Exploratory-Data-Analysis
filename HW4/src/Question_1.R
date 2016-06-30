# As per question I have changed 137.6 to 13.76
child <- c(1:18)
age <- c(109,113,115,116,119,120,121,124,126,129,130,133,134,135,137,139,141,142)
height <- c(13.76,147.8,136.8,140.7,132.7,145.4,135.0,133.0,148.5,148.3,147.5,148.8,133.2,148.7,152.0,150.6,165.3,149.9)

data <- cbind(as.matrix(child),as.matrix(age),as.matrix(height))
colnames(data) <- c("child","Age", "Height")
#data <- matrix(data = c(child,age,height),byrow=TRUE,nrow=18,ncol=3,dimnames()

threepointline1 <- function(x,y) {
  n <- length(x); nmod3 <- n%%3
  if(nmod3 == 0) {
    n3 <- n/3
  }
  if (nmod3 ==1) {
    n3 <- (n-1)/3
  }
  if (nmod3 ==3) {
    n3 <- (n+1)/3
  }  
  x.sort <- sort(x)
  y.sort <- sort(y)
  medxL <- median(x.sort[1:n3])
  medxR <- median(rev(x.sort)[1:n3])
  medyL <- median(y.sort[1:n3])
  medyR <- median(rev(y.sort)[1:n3])
  slopeb <- (medyR - medyL)/(medxR - medxL)
  medianx <- median(x)
  mediany <- median(y)
  intercepta <- ((medyL-slopeb*medxL) + (mediany-slopeb*medianx) + (medyR-slopeb*medxR))/3
  abline(intercepta,slopeb,col='red')
  meanx <- mean(x)
  meany <- mean(y)
  den <- 0
  num <- 0
  for (i in length(x)){
    den <- den + (x[i] - meanx)^2
    num  <- num + (x[i] - meanx)*(y[i] - meany)
  }
  slopels <- num/den
  interceptls <- meany - slopels*meanx
  abline(interceptls,slopels,col='green')
}
bartlett <- function(x,y){
  n <- length(x); nmod3 <- n%%3
  if(nmod3 == 0) {
    n3 <- n/3
  }
  if (nmod3 ==1) {
    n3 <- (n-1)/3
  }
  if (nmod3 ==3) {
    n3 <- (n+1)/3
  }  
  x.sort <- sort(x)
  n <- length(x); nmod3 <- n%%3
  if(nmod3 == 0) {
    n3 <- n/3
  }
  if (nmod3 ==1) {
    n3 <- (n-1)/3
  }
  if (nmod3 ==3) {
    n3 <- (n+1)/3
  }  
  x.sort <- sort(x)
  y.sort <- sort(y)
  meanxL <- mean(x.sort[1:n3])
  meanxR <- mean(rev(x.sort)[1:n3])
  meanyL <- mean(x.sort[1:n3])
  meanyR <- mean(rev(y.sort)[1:n3])
  meanx <- mean(x.sort)
  meany <- mean(y.sort)
  slopeb <- (meanyR - meanyL)/(meanxR - meanxL)
  interceptb <- meany - slopeb * meanx
  abline(interceptb,slopeb,col='blue')
}
plot(height~age,xlab="Age in months",ylab = " Height in cm",main="Age VS Height Plot")
threepointline1(age,height)
bartlett(age,height)

