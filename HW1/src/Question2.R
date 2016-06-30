#Function to plot a bivarient function and display the gradient
#for required points.

#List of Arguments:-
#1.expr:-Expression to be plotted.
#2.plotx:-X values to be plotted
#3.ploty:-Y values to be plotted
#4.arrowsx:-Values of x for which the gradient is to be plotted
#5.arrowsy:-values of y for which the gradient is to be plotted

plot_image = function(expr,plotx, ploty,arrowsx, arrowsy)
{
  # the Derivative function
  f = deriv(expr, c("x", "y"), function(x, y){})
  # Grid values of X and Y
  grid = expand.grid(plotx, ploty)
  # get the values of function for given X and Y
  z = f(grid[,1], grid[,2])
  # Matrix Form
  mat = matrix(z, nrow = length(plotx), ncol = length(ploty))
  #Image Plot
  image(plotx, ploty, mat, xlab = "X-AXIS", ylab = "Y-AXIS",
        main = expr)
  #Contour plot on the image plot itself
  contour(plotx, ploty, mat, add = TRUE)
  # grid values to plot the gradient
  arrowsgrid = expand.grid(arrowsx, arrowsy)
  # call the func passing the arrow grid value
  G = f(arrowsgrid[,1],arrowsgrid[,2])
  # get the gradient values
  grad = attr(G,"gradient")
  grad_tip = arrowsgrid + grad
  # get the X and Y gradient
  xgrad = grad[,1]
  ygrad = grad[,2]
  # Add Arrows to the plot
  arrows(arrowsgrid[,1], arrowsgrid[,2], grad_tip[, 1], 
         grad_tip[,2], lty = 5)
}
# define range for X
x = seq(-2*pi, 2*pi, length.out = 20)
# make Y equal to X
y = x
# define the X range values for plotting arrows
arrows_x = seq(-1.5*pi,1.5*pi, length.out = 10)
# make Y range equal to X range
arrows_y = arrows_x
# Expression
bivarient_expression = ~cos(x)*sin(y)
# Call Function
plot_image(bivarient_expression, x, x, arrows_x, arrows_x)
