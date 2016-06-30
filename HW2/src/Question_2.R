library(aplpack)

# Letter Value
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



A = c(19.50, 16.72, 20.92, 16.42, 21.22, 15.40, 20.68, 14.55, 20.23
      , 15.11, 20.95, 16.68, 14.67, 16.50, 22.15 , 20.14, 18.33, 14.20
      ,11.61,22.24,18.75,14.22,15.03,22.07,13.34,12.73,19.23
      ,19.74,19.74,20.60,19.29,18.22,23.65,17.44,13.07,19.00
      ,18.44, 17.25, 19.19, 12.77,14.10,16.69,16.92,21.92,20.84
      ,18.43, 19.54, 23.61, 21.40, 28.34, 20.43, 20.43, 15.58,16.58
      , 22.44, 14.59, 18.70, 16.79, 14.12, 13.67, 15.94, 24.04 , 15.42
      , 16.26, 17.74, 12.37 , 16.87, 16.28, 17.97, 19.56, 13.56, 16.13
      , 18.20, 17.79, 19.38, 20.47, 16.75, 16.69, 15.93, 14.73, 17.83
      , 19.78, 15.78, 16.17, 17.18, 13.90, 15.33, 16.10, 12.03,17.92
      , 23.56, 11.35, 19.10, 12.91, 18.32, 19.24, 11.57, 14.33, 13.60
      , 13.12, 11.19, 14.33, 16.91, 13.03, 17.32, 10.70, 12.56, 16.04)

# Plot Time series
b= ts(A, frequency = 12, start = c(1997, 1))
plot(b)

# Stem and Leaf
stem.leaf(b)


lval(b)

# QQ Plot
qqnorm(b)
qqline(b)

