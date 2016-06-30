time<- c(0.450,0.45,0.450,1.300,1.300,1.300,2.400,2.400,2.400,4.000,4.00,
         4.000,6.100,6.100,6.100,8.05,8.050,8.050,11.150,11.150,11.150,
         13.150,13.150,13.150,15.000,15.00,15.000)
cal<- c(0.342,0.00,0.825,1.780,0.954,0.641,1.751,1.275,1.173,3.123,2.61
        ,2.574,3.179,3.008,2.671,3.06,3.943,3.437,4.807,3.356,2.783,5.138
        ,4.703,4.257,3.604,4.15,3.425)
source("rrline.r")
rr=rrline1(time,cal)
# a)
inter=rr$a
slope=rr$b
inter
slope
# b) To straighten the plot, we need to do re-expression or perform
# transformation.
# c)
plot(time,cal)
plot(sqrt(time),cal)
abline(inter,slope)
# I have applied square root transformation

# d)
rr2=rrline1(sqrt(time),cal)
rr2
inter2=rr2$a
slope2=rr2$b
inter2
slope2
# e) 
# Panel c shows curvy trend and we can predict residuals.We need residual
# plot where residuals are scattered all over the place and we cannot
# find a trend then we can say that we have fit appropriate model.
# After performing transformation Panel d we can still see that most
# of the points are above the line and on bottom left there are no 
# residuals which implies that transformation performed is still not
#perfect
# f)
Slope3=0.256
Inter3=-0.124
newy= 0.256*sqrt(time)+Inter3
newy
rr3=rrline1(sqrt(time),newy)
rr3
slopenew=rr3$a
internew=rr3$b
slopenew
internew
