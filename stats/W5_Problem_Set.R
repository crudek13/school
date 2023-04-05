

#Q3
#t = x-bar - mu / s/sqrt

mu <- 5000 #pop mean
n <- 64 #sample
xbar <- 5160 #sample mean
sd <- 500 #sd

t <- (xbar - mu) / (sd/sqrt(n))
t


#Q4
pval <- 2*pt(-abs(t),df=n-1)
pval




#Q6
avg <- 5160
sd <- 500
n <- 64
Con <- .95

lower <- avg - qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)
upper <- avg + qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)

paste(lower, ",", upper)



#Q9

readspeed <- read.csv("readspeed.csv", header = TRUE)

mean(readspeed$Before)
mean(readspeed$After)


t.test(readspeed$After, readspeed$Before, paired = TRUE)



#Q15
avg <- 7.2
sd <- 12.5
n <- 15
Con <- .99

lower <- avg - qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)
upper <- avg + qt(1 - (1 - Con)/2, n - 1) * sd / sqrt(n)

paste(lower, ",", upper)








#Q18 - not paired - two sample independent - t.test

library(nycflights13)

air <- flights %>%
  filter(carrier %in% c("UA","DL"), 
         origin %in% c("JFK"), 
         dest %in% c("LAX"),
         !is.na(dep_delay))

t.test(dep_delay ~ carrier, data  = air)










