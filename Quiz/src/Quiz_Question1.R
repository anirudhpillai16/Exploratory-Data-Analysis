test1<- c(50,35,15,64,53,18,40,24,16,67,46,64,32,71,16)
test2<- c(58,46,40,76,62,39,57,41,31,75,62,64,54,65,51)
# a) Box Plot with Notches
boxplot(test1,test2,notch = TRUE,names= c("Test1","Test2"),col= c("blue","green"),
main= "Box Plot of two tests")
# Based on notches there is significant difference brtween the median values.
# Median for 2nd test is greater than 1st test
# b) Median, F-pseudo sigma and Standard Deviation of two tests1 
m1=median(test1)
m2=median(test2)
p1= IQR(test1)/1.35
p2= IQR(test2)/1.35
sd1= sd(test1)
sd2= sd(test2)
summary(test1)
summary(test2)
# c) Generating two new sets of tests with n=500
t1<- rnorm(500,m1,sd1)
t2<- rnorm(500,m2,sd2)
# d) Notched Box Plots to Compare two new datasets
boxplot(t1,t2,notch = TRUE, names= c("Test1","Test"),col= c("blue","green"),
        main= "New Box Plot of two sets")
# e) Letter value Boxplots for Test
source("C:/Users/lenovo/Documents/lvalprogs.R")
lval(t1)
lval(t2)
lvplot(t1,main="Letter value plot for Test1")
lvplot(t2,main="Letter value plot for Test2")
