x = seq(0.01,0.99,by=0.02)
y=c(-0.0937,0.0247,0.1856,0.1620,-0.0316, 0.1442,0.0993,0.3823,-0.0624,
    0.3262,0.1271,-0.4158,0.0975,-0.0836,0.7410,0.3749,0.4446,0.5432,0.6946
    ,0.5869,0.9384,0.7647,0.9478,0.9134,1.2437,0.9070,1.2289,0.9638,0.8834,
    0.6982,0.5729,0.7160,1.0083,0.6681,0.5964,0.4759,0.6217,0.6221,0.6244,
    0.5918,0.7047,0.5234,0.9022,0.9930,0.8045,0.7858,1.1939,0.9272,0.8832,
    0.9751)
mu = function(t){t + 0.5 *exp(-50*(t-0.5)^2)}
curve(mu(x),0,1)
points(x,y)
kernel_cv = function(x, y, lam)
{
  n = length(x)
  cv=numeric(n)
  for(i in 1:n)
  {
    fit = ksmooth(x[-i], y[-i], kernel = "normal", bandwidth = lam, n.points = n)
    cv[i] = (y[i] - fit$y[i])^(2)
  }
  cv = mean(cv, na.rm = T)
  return(cv)
  
}

spline_cv = function(x, y, lam)
{
  fit = smooth.spline(x, y,spar = lam ,cv=TRUE)
  return(fit$cv.crit)
}

spline_gcv = function(x, y, lam)
{
  fit = smooth.spline(x, y,spar = lam ,cv=FALSE)
  return(fit$cv.crit)
}

# part f

plot_cv = function(x, y, spline = 0)
{
  lam = seq(0.01, 1, by = 0.01)
  cv3 = c()
  for(i in 1:length(lam))
  {
    if(spline == 0)
    {
      ylab = "CV"
      cv2 = kernel_cv(x, y, lam[i])
      cv3 = c(cv3, cv2)
    }
    else
    {
      if(spline == 1)
      {
        ylab = "CV"
        cv2 = spline_cv(x, y, lam[i])
        cv3 = c(cv3, cv2)
      }
      else
      {
        ylab = "GCV"
        cv2 = spline_gcv(x, y, lam[i])
        cv3 = c(cv3, cv2)
      }
    }
    
  }
  plot(lam, cv3, xlab = "Lambda", ylab = ylab, main = "Lambda vs cross validation", pch = 20, col = "green")
  lam_min = lam[which.min(cv3)]
  return(lam_min)
}

plot_ksmooth = function(x, y, mu, method)
{
  lam = c(0.1, 0.3, 0.5, 0.7, 0.9)
  col = c("red", "yellow", "green", "blue", "orange")
  titl = paste("Plot for different bandwidth:", method)
  curve(mu(x), 0, 1, main = titl)
  points(x, y)
  for(i in 1:length(lam))
  {
    fit = ksmooth(x, y, kernel = method, bandwidth = lam[i])
    lines(fit$x, fit$y, lty = 4, col = col[i])
    
  }
  legend('bottomright', legend = lam , lty=1, col= col, bty='n', cex=.75)
}

x = seq(0.01, 0.99, by = 0.02)
y = c(-.0937, .0247, .1856, .1620, -.0316, .1442, .0993, .3823, -.0624, .3262, .1271, -.4158, .0975, -.0836, .7410, .3749, .4446, .5432, .6946, .5869, .9384, .7647, .9478, .9134, 1.2437, .9070, 1.2289, .9638, .8834, .6982, .5729, .7160, 1.0083, .6681, .5964, .4759, .6217, .6221, .6244, .5918, .7047, .5234, .9022, .9930, .8045, .7858, 1.1939, .9272, .8832, .9751)

#plot(x, y, col = "purple")
mu = function(t){t + 0.5 *exp(-50*(t-0.5)^2)}
u = expression(t + 0.5 *exp(-50*(t-0.5)^2))
curve(mu(x), 0, 1)
points(x, y)

# part b
plot_ksmooth(x, y, mu, "Box")
plot_ksmooth(x, y, mu, "Normal")

# part c
lam_min = plot_cv(x, y, spline = 0)
ans = paste("The Optimized Lambda Value is", lam_min)
print(ans)

# part e

plot_spline = function(x, y, mu)
{
  lam = c(0.1, 0.3, 0.5, 0.7, 0.9)
  col = c("red", "yellow", "green", "blue", "orange")
  curve(mu(x), 0, 1, ylim = range(-0.6, 1.5), main = "Smoothing Spline")
  points(x, y)
  for(i in 1:length(lam))
  {
    fit = smooth.spline(x, y, spar = lam[i])
    lines(fit$x, fit$y, lty = 4, col = col[i])
  }
  legend('bottomright', legend = lam , lty=1, col= col, bty='n', cex=.75)
}

plot_spline(x, y, mu)
min_lam = plot_cv(x, y, spline = 1)
min_gcv = plot_cv(x, y, spline = 2)
