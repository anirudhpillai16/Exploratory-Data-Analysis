#3 a)
LVhouseHold_data <- cbind(c(rep(0, 10)),c(0, 2412, 1788, 1517, 1248, 963.5, 727.5, 579,345,114),c(3480,3678,4115.5,4400.5,4799,4978.75,5241,5394.5,5510.25,5494),c(0,4944,6443,7284,8350,8994,9754.5,10210,10675.5,10874));

#x0 = 3480, z = a*x^(1/3) + b, dz/dx = (1/3)a*x^(-2/3), Now, at x0 = 3480 , a = 688.9284 , b = -6960 Hence, z = 688.9284* x^(1/3) - 6960

# b)
Transformed_value <- 688.9284* (LVhouseHold_data)^(1/3) - 6960
Transformed_value

# c)
# Mids are almost same as compared to data in table 4-5.But mids are very
# far seperated compared to log, square root, fourth root transform,
# By Comparing 25th and 75th quantiles, it has moved away from  orignal
# data. Spread has increased compared to other transforms hence we can
# easily identify any outliers.