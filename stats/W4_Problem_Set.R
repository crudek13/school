


#Q3
dbinom(5,50,.1)


#Q4
pbinom(9, 260, .05)


#Q5
pnorm(99,530,205)

#Q6
qnorm(.75,530,205)


#Q7
#detect is greater than 150. fail to detect is less than 150
pnorm(150,160,20)


#Q8
pnorm(150,160,20/sqrt(5))


#Q9
pnorm(2799,3000,500)

#10 - no longer normal so 
pbinom(20,52,0.343842)


#Q13
pbinom(60,120,.6)


(1/6)*1 + (1/6)*2 + (1/6)*3 + (1/6)*4 + (1/6)*5 + (1/6)*6 


#Q16
(2/60)*10


#Q17, diff ways to do it
ppois(0, lambda = .333, lower.tail = FALSE)
1 - ppois(0, lambda = .333)

1 - dpois(0,lambda=.333)


#Q20 - similiar to 8 and 10
#.017?

pnorm(24,20,10,lower.tail=FALSE)


