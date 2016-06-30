data = read.table("ceo.txt", header = T)
# a) Number of CEO's and Highest Paid CEO's
n=length(data$TotalCompensation)
max_sal=max(data$TotalCompensation)
print(max_sal)
print(n)
boxplot(data,horizontal = TRUE,col="red")
# There are many Outliers which can be said to be unusual values.
# b) Graphical Display for Data
hist(data[,1])
# Distribution is skewed to the right and I would like to transform the 
#data.

# c) Cube root transform will be more appropriate as it will make data
#symmetric and can resemble normal distribution.

# d) We need to remove low valued data also few Ceo with salary 0 as it 
# affect mean.
summary(data)
clear_data<- data[data[,1]> 10000,]
summary(clear_data)
# e) 
hist(data[,1])
# Square root Transform
hist(sqrt(data[,1]))
# Cube root Transform
hist(data[,1]^(1/3))
# Log Transform
hist(log(data[,1]))

# Transform after removing outliers
hist(clear_data)
hist(sqrt(clear_data))
hist(clear_data^(1/3))
hist(log(clear_data))
# As you can see data has become more symmetric when cube root transform
#is applied.

# f) I would choose cube root transform as it makes data symmetric and 
#less skewed compared to log or square root transform
