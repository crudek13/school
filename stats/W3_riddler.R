#one time of money going left, right, or game ending

# position defaults to 0, money starts in front of me
moneycircle <- function(position = 0) {
  
  result <- 1
  
  while(result != 0) {
    result <- sample(c(-1,1,0), 1)
    position <- position+result
    #print(paste("result =", result))
    #print(paste("Position =", position))
  }
  
  if (position %% 5 == 0) {
    payoff <- 100
  } else {
    payoff <- 0
  }
  
  
  print(payoff)
  
}



set.seed(123)
payouts <- rep(0,10000)
for(rep in 1:10000) {
  payouts[rep] <- moneycircle()
}
head(payouts)
mean(payouts)



mean(replicate(n=10000, moneycircle()))

mean(replicate(n=10000, moneycircle(position = -1)))

mean(replicate(n=10000, moneycircle(position = 1)))
