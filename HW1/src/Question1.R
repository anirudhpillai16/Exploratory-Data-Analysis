plot_image=function(x,y)
{
  value_range=expand.grid(x,y)
  z_matrix=matrix(cos(value_range$Var1)*sin(value_range$Var2),nrow=length(x))
  image(x,y,z_matrix)
}
x=seq(-pi,pi,length = 10)
y=x
plot_image(x,y)
