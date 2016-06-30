
myplotfit <- function(outmpol,outlim=0,...) {
# outmpol is output of medpolish in library(eda) or library(stats)
# be sure to assign dimnames to matrix being polished
oldpar <- par()
par(fig=c(0,.7,0,1))
nc <- length(outmpol$col)
nr <- length(outmpol$row)
a <- rep(outmpol$row,nc)
b <- rep(outmpol$col,rep(nr,nc)) 
sqrt2 <- sqrt(2)
ab <- cbind((a-b)/sqrt2,(a+b)/sqrt2)
xrange <- range(ab[,1]) + c(-.1,.1)*(max(ab[,1])-min(ab[,1]))
yrange <- range(ab[,2]) + c(-.1,.1)*(max(ab[,2])-min(ab[,2]))
dx <- (xrange[2]-xrange[1])/50
dy <- (yrange[2]-yrange[1])/50
plot(ab[,1],ab[,2],axes=F,xlim=xrange,ylim=yrange,xlab="",ylab="",...)
segments((min(a)-outmpol$col)/sqrt2, (min(a)+outmpol$col)/sqrt2,
         (max(a)-outmpol$col)/sqrt2, (max(a)+outmpol$col)/sqrt2,lty=3)
segments((outmpol$row-min(b))/sqrt2, (outmpol$row+min(b))/sqrt2,
         (outmpol$row-max(b))/sqrt2, (outmpol$row+max(b))/sqrt2,lty=3)
# segments((outmpol$row)/sqrt2-min(b), (outmpol$row)/sqrt2+min(b),
#        (outmpol$row)/sqrt2-max(b), (outmpol$row)/sqrt2+max(b),lty=3)
yrowloc <-  rep(max(b),nr)
xrowloc <-  outmpol$row
# text((xrowloc-yrowloc)/sqrt2-dx,dy+(xrowloc+yrowloc)/sqrt2,format(1:nr))
text((xrowloc-yrowloc)/sqrt2-3*dx,dy+(xrowloc+yrowloc)/sqrt2,
  names((xrowloc-yrowloc)/sqrt2-dx))
xcolloc <- rep(max(a),nc)
ycolloc <- outmpol$col
# text(dx+(xcolloc-ycolloc)/sqrt2,dy+(xcolloc+ycolloc)/sqrt2,format(1:nc))
text(3*dx+(xcolloc-ycolloc)/sqrt2,dy+(xcolloc+ycolloc)/sqrt2,
  names(dx+(xcolloc-ycolloc)/sqrt2))
ynames <- format(round(outmpol$overall + sqrt2*pretty(ab[,2])))
axis(2,at=pretty(ab[,2]),labels=ynames)
# add vertical lines when there is an outlier
if(abs(outlim) > 1e-4) {
   out.index <- which(abs(outmpol$res) > outlim, arr.ind=T)
   # find (r,c) for outlier indices
   zz.x <- outmpol$row[out.index[,1]]
   zz.y <- outmpol$col[out.index[,2]]
   # outlier points at (zz.x-zz.y)/sqrt2, (zz.x+zz.y)/sqrt2
   # draw segment from here to end of residual
   segments((zz.x-zz.y)/sqrt2, (zz.x+zz.y)/sqrt2,
         (zz.x-zz.y)/sqrt2, (zz.x+zz.y)/sqrt2 + outmpol$res[out.index])
}
par <- oldpar
invisible()
}

