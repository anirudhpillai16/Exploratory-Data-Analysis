
# Letter value plots & qqplots based on LVs (Karen Kafadar 2006)

lval <- function(x) {
 #tag <- c("M ","F ","E ","D ","C ","B ","A ","Z ","Y ","X ","W ","V","U","T",
 # "S","R","Q","P","O","N")
 # gau <- abs(qnorm(c(.25,.125,1/16,1/32,1/64,1/128,1/256,1/512,1/1024,1/2048,
 #    1/4096, 1/8192, 1/16384, 1/32768, 1/65536)))
  tag <- c("M",LETTERS[6:1],LETTERS[26:14])

  gau <- abs(qnorm(1/2^(2:20)))

# col 1 = depth; 2 = lower; 3 = upper; 4 = mid; 5 = spread; 6 = pseudo-s

  y <- sort(x[!is.na(x)])
  n <- length(y)
  m <- ceiling(log(n)/log(2)) + 1
  depth    <- rep(0,m)
  depth[1] <- (1 + n)/2

  for (j in 2:m) {depth[j] <- (1 + floor(depth[j-1]))/2 }

  ndepth <- n+1 - depth
  out <- matrix(0, m, 6)
  dimnames(out) <- list(tag[1:m],
          c("Depth", "Lower","Upper", "Mid", "Spread","pseudo-s"))
  out[1,2:3] <- median(y)
  out[,1] <- depth


  for (k in 2:m) {
    out[k,2] <- ifelse(depth[k] - round(depth[k]) == 0,
                y[depth[k]], (y[depth[k]-.5]+y[depth[k]+.5])/2 )
    out[k,3] <- ifelse(ndepth[k] - round(ndepth[k]) == 0,
                y[ndepth[k]], (y[ndepth[k]-.5]+y[ndepth[k]+.5])/2 )
  }

  out[1:m,4] <- (out[1:m,2] + out[1:m,3])/2
  out[2:m,5] <- out[2:m,3] - out[2:m,2]
  out[2:m,6] <- out[2:m,5]/(2*gau[1:(m-1)])
  round(out,4)
}




lval.sub <- function(x) {
  tag <- c("M",LETTERS[6:1],LETTERS[26:14])
  # gau <- abs(qnorm(1/2^(2:20)))

# returns a vector (..., LE, LF, M, M, UF, UE, ...) 
# of letter values only (not pseudo-spreads, etc)

  y <- sort(x[!is.na(x)])
  n <- length(y)
  m <- ceiling(log(n)/log(2)) + 1
  depth    <- rep(0,m)
  depth[1] <- (1 + n)/2

  for (j in 2:m) {depth[j] <- (1 + floor(depth[j-1]))/2 }

  ndepth <- n+1 - depth
  out <- matrix(0,m,3)
  dimnames(out) <- list(tag[1:m], c("Depth", "Lower","Upper"))
  out[1,2:3] <- median(y)
  out[,1] <- depth

  for (k in 2:m) {
    out[k,2] <- ifelse(depth[k] - round(depth[k]) == 0,
                y[depth[k]], (y[depth[k]-.5]+y[depth[k]+.5])/2 )
    out[k,3] <- ifelse(ndepth[k] - round(ndepth[k]) == 0,
                y[ndepth[k]], (y[ndepth[k]-.5]+y[ndepth[k]+.5])/2 )
  }

  # c(rev(out[,2]),out[,3])
  out
}



stackbox <- function(x,k=7,...) {

# extension of standard boxplots  Heike Hofmann, 2005
# draws letter statistics up to level k

  plot(x,rep(k/2,length(x)),ylim=c(0,k),ylab="",axes=F,type="n",...)
  box()
  axis(1)
  n <- length(x)
  qu <- quantile(x, c(.5^(k:1),1-.5^(1:k)))    # based on quantiles, not LVs
                                               # that's easy to change, though
  # determine & draw outliers
  out <- (x<qu[1]) | (x>qu[2*k])
  points(x[out],rep(k/2,sum(out)))

  # draw boxes:
  
  for (i in 1:k)
    rect(qu[i], (k+i)/2,qu[2*k-i+1], (k-i)/2, col="grey")
}



lvplot <- function(x,tag,ylabname="", ...) {
# Karen Kafadar, 2006
# this may be slow for many groups 
  if(missing(tag)) tag <- rep(1,length(x))
  y <- x[!is.na(x)]
  tag <- tag[!is.na(x)]
  groupid <- unique(tag)
  ngroup <- length(groupid)
  ni <- table(tag)
  xlabname <- as.character(groupid)
  plot(c(0.5,ngroup+0.5),range(y,na.rm=T),xlab="",axes=F,type="n",
           ylab=ylabname,...)
  box()
  axis(1,at=1:ngroup,labels=xlabname)
  axis(2,at=pretty(y))
  for (j in 1:ngroup) {
    x <- y[tag==groupid[j]]
    n <- ni[j]
# ensure that k is at least 2, so at least fourths are shown
    k <-  1+max(2, 1 + ceiling(log(n,2) - log(4*1.96^2,2)))
    lval.x <- lval.sub(x)
    qu <- c(rev(lval.x[1:k,2]),lval.x[1:k,3])
    med <- qu[k]
    lfence <- 4*lval.x[2,2] - 3*lval.x[2,3]
    ufence <- 4*lval.x[2,3] - 3*lval.x[2,2]
    lfourth <- lval.x[2,2]
    ufourth <- lval.x[2,3]
    lower.adj <- ifelse(min(x) < lfence, min(x[x > lfence]), min(x))
    upper.adj <- ifelse(max(x) > ufence, max(x[x < ufence]), max(x))
    # draw boxes:
    wid <- 1/2^(k:1) - 0.01
    if (n < 30) {   # i.e., k=2: draw boxplot
      if (n < 10) {
         points(rep(j,n),x)
         segments(j-.49, med, j+.49, med)  # line for median
      }
      else {
         rect(j-wid[k-1], lfourth, j+wid[k-1], ufourth, col="grey")
         segments(j-.49, med, j+.49, med)  # line for median
         out <- (x < lower.adj) | (x > upper.adj)
         points(rep(j,sum(out)),x[out])
         segments(j,lfourth,j,lower.adj,lty=2)
         segments(j,ufourth,j,upper.adj,lty=2)
     }
    }
    else {
      # out <- (x < min(lfence, qu[1])) | (x > max(ufence, qu[2*k]))
      out <- (x < qu[1]) | (x > qu[2*k])
      points(rep(j,sum(out)),x[out])
      for (i in 1:k) 
      rect(j-wid[i], qu[i], j+wid[i], qu[2*k-i+1], col="grey")
    }
  }
}

qqnormLV <- function(y, main="Normal QQ Plot (LVs)", 
       xlab="theoretical letter values",
       ylab="sample letter values", plot.it = TRUE, ...) {
  # QQ plot on letter values only (Karen Kafadar, 2006)
  lv.out <- lval(y[!is.na(y)])
  n <- length(y[!is.na(y)])
  yLV <- c(rev(lv.out[,2]),lv.out[-1,3])
  zLV <- qnorm((c(rev(lv.out[,1]),n+1-lv.out[-1,1]) - 1/3)/(n+1/3))
  plot(zLV,yLV,main=main, xlab=xlab, ylab=ylab)
  # slope of line through 1st & 3rd quartiles:
  slope.lv <- (lv.out[2,3]-lv.out[2,2])/1.349
  int.lv <- lv.out[2,3] - slope.lv*.6745
  abline(int.lv,slope.lv)
  if(!plot.it) list(x = zLV, y=yLV)
}



plot.sym <- function(x) {
# Plot for transformations for symmetry UREDA Sec 4C, p.105
# Note: if the plot is not straight, transform x and try again
y <- x[!is.na(x)]
ll <- lval(y)
mm <- ll[1,2]  # median
m <- nrow(ll)
xx <- ( (ll[2:m,3]-mm)^2 + (mm-ll[2:m,2])^2 )/(4*mm)
yy <- ( ll[2:m,3] + ll[2:m,2] )/2 - mm
plot(xx,yy)
rr <- lm(yy ~ xx)
print(rr$coef)
abline(rr)
pwr <- 1 - rr$coef[2]
title("",paste("Approximate power =",format(round(pwr,2))))
}

