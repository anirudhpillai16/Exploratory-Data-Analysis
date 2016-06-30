library(noncensus)
data(counties)

Median_spread_level = function(a)
{
  a = sort(a)
  m_ind = (length(a)+1)/2
  median = ifelse(m_ind-floor(m_ind)==0, a[m_ind], (a[m_ind+0.5]+a[m_ind-0.5])/2)
  fl_ind = (floor(m_ind)+1)/2
  fl = ifelse(fl_ind - round(fl_ind)==0, a[fl_ind], a[fl_ind+0.5]/2+a[fl_ind-0.5]/2)
  fu_ind = length(a) - fl_ind + 1
  fu = ifelse(fu_ind-floor(fu_ind)==0, a[fu_ind], a[fu_ind+0.5]/2+a[fu_ind-0.5]/2)
  return (c(median, fl, fu, fu-fl))
}

spread_level = function()
{
  states = levels(counties$state)
  median = c()
  spread = c()
  for(st in states)
  {
    state_data = counties[counties$state==st,]
    info = Median_spread_level(state_data$population)
    median = c(median, info[1])
    spread = c(spread, info[4])
    
  }
  median = log10(median)
  spread = log10(spread)
  print(median)
  median = na.omit(median)
  spread = na.omit(spread)
  print(spread)
  plot(median, spread, col = "blue")
  title("Spread vs Level Plot")
  text(median, spread,states, cex = 0.65, offset = 0, col = "brown", pos = 3)
  fit = lm(spread~median)
  abline(fit)
  coef(fit)
}

spread_level()
