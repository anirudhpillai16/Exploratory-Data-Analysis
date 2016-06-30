library(MASS)
data(pressure)
attach(pressure)
y=pressure$pressure
x=pressure$temperature

y= y * 0.1333
x=x + 273.15
# a) Scatter Plot of Pressure versus Temperature
plot(x,y,main = "Pressure versus Temperature",xlab="Temperature in Kelvin",
     ylab="Pressure in Kilo Pascals")
# b) Let us try Log, Square root and Cube root transform
plot(y~log(x),main="Log Transform")
plot(y~sqrt(x), main="Square root Transform")
plot(y, x^(1/3))
# c) 
plot(y~I(1/x))
# e) Box Cox Transformation
boxcox(y~I(1/x))
plot(log(y)~I(1/x))

