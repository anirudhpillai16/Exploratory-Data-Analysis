source("rrline.r")
X1 = c(61, 54, 70, 81, 46, 63, 54, 44, 53, 60, 80, 25, 59, 42, 34, 46, 63, 62, 67, 54, 31, 54, 31, 26, 41, 34, 42, 82, 202, 89, 84, 133, 85, 51, 91, 45, 102, 48, 60)
X2 = c(38, 47, 40, 40, 36, 37, 37, 35, 36, 42, 52, 34, 37, 42, 37, 37, 37, 39, 39, 41, 37, 38, 37, 43, 39, 34, 35, 44, 57, 46, 61, 50, 35, 37, 37, 37, 38, 33, 37)
X3 = c(127, 213, 134, 131, 116, 127, 128, 114, 123, 130, 177, 104, 113, 112, 91, 108, 126, 122, 121, 122, 96, 118, 88, 95, 101, 90, 101, 125, 231, 145, 214, 158, 148, 114, 144, 113, 151, 106, 120, 88)
X4 = c(144, 152, 149, 219, 174, 171, 178, 125, 213, 191, 194, 96, 167, 118, 165, 135, 178, 243, 107, 176, 109, 146, 104, 70, 156, 157, 136, 257, 276, 141, 177, 140, 198, 154, 152, 119, 171, 151, 204)
X5 = c(50, 37, 40, 67, 48, 50, 85, 58, 78, 52, 59, 19, 45, 32, 31, 41, 57, 48, 64, 63, 46, 41, 31, 15, 38, 49, 49, 108, 153, 124, 138, 88, 54, 72, 70, 40, 57, 84, 42, 15)
y = c(92, 163, 215, 210, 128, 239, 317, 195, 108, 158, 263, 101, 266, 290, 169, 168, 146, 145, 195, 254, 99, 146, 132, 105, 131, 198, 237, 202, 461, 350, 275, 467, 201, 169, 253, 202, 195, 140, 179, 467)

fit1 = run.rrline(X1, y)
c1 = fit1$coef[6,2]
y.1 = y - c1*X1

fit2 = run.rrline(X2, X1)
d21 = fit2$coef[6,2]
X2.1 = X2 - d21*X1

fit3 = run.rrline(X2.1, y.1)
c2 = fit3$coef[6,2]
y.12 = y.1 - c2*X2.1

fit4 = run.rrline(X4, X1)
d41 = fit4$coef[6, 2]
fit5 = run.rrline(X4, X2.1)
d421 = fit5$coef[6, 2]
X.124 = X2.1 - d421*X4
fit6 = run.rrline(X4, y.12)
c421 = fit6$coef[6, 2]
y.124 = y.12 - c421*X.124

b0 = median(y.124)
b0
b1 = c1 - c2*d21 - c421*d421
b2 = c2 - c421*d421
b3 = c421
b1
b2
b3