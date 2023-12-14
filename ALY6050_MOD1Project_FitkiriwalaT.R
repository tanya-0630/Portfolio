print("Tanya Fitkiriwala")

##part 1
#win amount = 500
#loss amount = 520
#net win = 102.6

## winning home game variance
g1 = (500 - 102.6)**2 * 0.6
g1
## losing home game variance
g2 = (520 - 102.6)**2 * 0.4
g2
## winning game in other city variance
g3 = (500 - 102.6)**2 * 0.43
g3
## losing game in other city variance
g4 = (520 - 102.6)**2 * 0.57

##standard deviation
sqrt(g1 + g2 + g3 + g4)


##expected net win/loss for the entire season

a <- c(500, -520)
b<- c(0.54, 0.46) # average wins and loss

exp <- crossprod(a,b)
exp

#sd'
s <- sqrt(crossprod(a**2,b) - (crossprod(a,b))**2)
s



###observed wins and losses using random numbers
set.seed(456)
Y = sample(c(-520,500), prob = c(0.46,0.54), 10000, replace = TRUE)
mean = mean(Y)
sd = sd(Y)

mean
sd

n <- 10000
error <- qnorm(0.95)*sd/sqrt(n)
left <- mean - error
right <- mean + error

left
right

#frequency distribution
Freq = table(Y)
prob <- prop.table(Freq)
prob

#chi square test
chisq.test(c(4609,5391), p = c(0.46,0.54))

##part 2
#win amount = 500
#loss amount = 520
#net loss = -70.8

## winning home game variance
g1 = (500 - (-70.8))**2 * 0.6
g1
## losing home game variance
g2 = (520 - (-70.8))**2 * 0.4
g2
## winning game in other city variance
g3 = (500 - (-70.8))**2 * 0.43
g3
## losing game in other city variance
g4 = (520 - (-70.8))**2 * 0.57
g4

##standard deviation
sqrt(g1 + g2 + g3 + g4)


##expected net win/loss for the entire season

a <- c(500, -520)
b<- c(0.49, 0.51) # average wins and loss

exp <- crossprod(a,b)
exp

#sd'
s <- sqrt(crossprod(a**2,b) - (crossprod(a,b))**2)
s



###observed wins and losses using random numbers
set.seed(456)
Y = sample(c(-520,500), prob = c(0.51,0.49), 10000, replace = TRUE)
mean = mean(Y)
sd = sd(Y)

mean
sd

n <- 10000
error <- qnorm(0.95)*sd/sqrt(n)
left <- mean - error
right <- mean + error

left
right

#frequency distribution
Freq = table(Y)
prob <- prop.table(Freq)
prob

#chi square test
chisq.test(c(5063,4937), p = c(0.51,0.49))


###part 3
##best of 3

#win amount = 500
#loss amount = 520
#net win = 113.2

## winning home game variance
g1 = (500 - 113.2)**2 * 0.6
g1
## losing home game variance
g2 = (520 - 113.2)**2 * 0.4
g2
## winning game in other city variance
g3 = (500 - 113.2)**2 * 0.43
g3
## losing game in other city variance
g4 = (520 - 113.2)**2 * 0.57
g4

##standard deviation
sqrt(g1 + g2 + g3 + g4)


##expected net win/loss for the entire season

a <- c(500, -520)
b<- c(0.532, 0.468) # average wins and loss

exp <- crossprod(a,b)
exp

#sd'
s <- sqrt(crossprod(a**2,b) - (crossprod(a,b))**2)
s



###observed wins and losses using random numbers
set.seed(456)
Y = sample(c(-520,500), prob = c(0.468,0.532), 10000, replace = TRUE)
mean = mean(Y)
sd = sd(Y)

mean
sd

n <- 10000
error <- qnorm(0.95)*sd/sqrt(n)
left <- mean - error
right <- mean + error

left
right

#frequency distribution
Freq = table(Y)
prob <- prop.table(Freq)
prob

#chi square test
chisq.test(c(4712,5288), p = c(0.468,0.532))


