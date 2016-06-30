# a) 
#Data is left skewed and transformation would be make data symmetric.
# X= ((d_u)^2 + (d_l)^2) / (4M) , Y = ((d_u) - (d_l)) / 2 
# Where (d_u) and (d_l) = distance from (x_l) and (x_u) to M. 
# Using this we will now calculate the slope and then the value 
#of power transform.
# b)
# Spreads are large and it's necessary to transform data to do better
# analysis of spread versus level plot and will give good estimate of
# mid summaries and accordingly we can use transformation.
# X= log M, Y= log(d_f) is F-spread and M is the median for all batches.
# c)
# Since there are heavy tails which won't go even after transformation hence
# transformation will not be a good idea.
# d)
# There is one heavy tail and one light tail hence we can say that there
# is skewness in data hence we need to perform transformation since it is
# single batch we will plot transformation plot between
#  X= ((d_u)^2 + (d_l)^2) / (4M) , Y = ((d_u) - (d_l)) / 2 
# Where (d_u) and (d_l) = distance from (x_l) and (x_u) to M.
# We can know the power transformation.
