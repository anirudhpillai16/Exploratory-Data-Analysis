# a)
T1<-c(5,6,3,11,10)
T2<-c(14,10,6,12,21)
T3<-c(16,24,15,26,32)
coln<- c("A1","A2","A3","A4","A5")
flyinsctble<-rbind(T1,T2,T3)
colnames(flyinsctble)<-coln
flyinsctble.MP<-medpolish(flyinsctble)
flyinsctble.MP
# Row Effects shows that Flying insects visits T3 (Pink) most followed by
# white which is second most visisted and then red which is least visited.
# Column effect shows that insects visit A5 most then A4 then A2, then A1
# lastly A3. A5 > A4 >A2 > A1 > A3
# b) 
sum.residual<- sum(abs(flyinsctble.MP$residuals))
sum.data <- sum(abs(flyinsctble - flyinsctble.MP$overall))
Analog.RSqr<- 1-(sum.residual/sum.data)
Analog.RSqr
# c)
# Diagnositc Plot is plot between residuals and comparision values.The
# Plot reveals trends or patterns and helps us understand if data is
# systematic or not from additive model.
# Before plotting diagnositc plot, we should have analyzed residuals
# and know that there is a problem with non-additivity and we already
# expect trends. The slope helps us in transformation to remove
# non-additivity.
# We plot the diagnostic plot between the residuals on y-axis and 
# comparision values on x-axis.
source("myplotfit.r")
myplotfit(flyinsctble.MP)
#We see that the affect of (T3,A4) = 26 and (T2,A5) nearly 24 are 
#almost similiar, they have the same affect.