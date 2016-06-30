library(cluster)
vote = votes.repub
par(mfrow = c(3, 2))
vote_calc = function(vote, reg, name)
{
  
  colours=c("blue","black","red","deeppink","red","purple","green","brown",
            "orange","violet")
  par(mar=c(4,5,0,2))
  j = 1
  for(i in reg)
  {
    vote_t = vote[i,]
    vote_t = as.data.frame(t(vote_t))
    vote_ts = ts(vote_t, frequency = 0.25, start = 1856)
    if(i==reg[1])
    {
      plot(vote_ts, col = colours[1], ylim = range(0,100),
           ylab = name)
    }
    else
      j = j + 1
    lines(vote_ts, col = colours[j])
  }
  segments(1852,50,1980,50, col = "grey65", lty = 2)
}

northeast = c("Connecticut","Delaware","Maine", "Massachusetts","New Hampshire","New Jersey",
              "New York","Pennsylvania","Rhode Island","Vermont")
east_central = c("Kentucky","Maryland","North Carolina","South Carolina","Tennessee",
                 "Virginia","West Virginia")
south = c("Alabama", "Arkansas", "Florida", "Georgia","Louisiana","Mississippi","Oklahoma","Texas")
midwest = c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota","Missouri","Nebraska",
            "Ohio","Wisconsin")
rockies = c("Colorado","Idaho","Montana","North Dakota","South Dakota","Utah","Wyoming")
west = c("Alaska","Arizona","California","Hawaii","Nevada","New Mexico","Oregon","Washington")

# Box Plot for All States
par(mfrow=c(1,1))
boxplot(t(votes.repub), main ="All States")
qqnorm(t(votes.repub), main ="All States")

par(mfrow=c(3,2))
# Box Plot for Northwest
boxplot(t(votes.repub[northeast,]))

# Box Plot for East central
boxplot(t(votes.repub[east_central,]))

#Box Plot for South
boxplot(t(votes.repub[south,]))

#Box Plot for Midwest
boxplot(t(votes.repub[midwest,]))

# Box Plot for Rockies
boxplot(t(votes.repub[rockies,]))

# Box Plot for West
boxplot(t(votes.repub[west,]))


par(mfrow=c(3,2))
qqnorm(t(votes.repub[northeast,]), main ="North East")
qqnorm(t(votes.repub[east_central,]), main = "East central")
qqnorm(t(votes.repub[south,]), main="South")
qqnorm(t(votes.repub[midwest,]), main="Midwest")
qqnorm(t(votes.repub[rockies,]), main="Rockies")
qqnorm(t(votes.repub[west,]), main="West")
