infant_mortality<-c(25.3,32.1,38.8,25.4,25.3,29.0,31.0,21.1,18.2,18.8,19.3,20.3,18.3,24.3,15.7,24.0,16.3,19.0,16.8,17.5)
infant_mortality<-matrix(infant_mortality,c(4,5))
dimnames(infant_mortality)<-list(c("NorthEast","North Central","South","West"), c("<8","9-11","12","13-15",">16"))
mat<-infant_mortality
twoway.median <- function(mat){ 
  meff.MP <- median(mat)
  beff.MP <- apply(mat,2,median,na.rm=T)  
  mat.res <- mat - matrix(rep(beff.MP,each=nrow(mat)),byrow=F,nrow=nrow(mat)); 
  
  beff.MP <- beff.MP - median(beff.MP)  
  
  aeff.MP <- apply(mat.res,1,median,na.rm=T)
  
  res.MP <- mat.res - matrix(rep(aeff.MP,each=ncol(mat)),byrow=T,ncol=ncol(mat)) 
  
  list(overall=meff.MP, row=aeff.MP, col=beff.MP, res=res.MP)
}
iter1 <- twoway.median(mat); #1st iteration
iter2<-twoway.median(iter1$res) #2nd iteration
mat<-rbind(iter2$res,iter2$col)
mat<-cbind(mat,iter2$row)
mat[5,6]<-iter2$overall
rownames(mat)[5]<-"col effect"
colnames(mat)[6]<-"row effect"
mat
# There is a difference when compared to table with row and column effect
#hence it depends if we start with row or column.
