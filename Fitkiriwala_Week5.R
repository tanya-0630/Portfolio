print("Tanya Fitkiriwala")

####sign test#######

###Game attendance###

#hypothesis: median = 3000

#set significance level
alpha <- 0.05

#set the claim
claim <- 3000

#critical value
n <- 20
k <- round(((n-1)/2)-0.95*(sqrt(n)))
cat("Critical value = ", k)

#20 values taken at random
attendance <- c(6210,3150,2700,3012,4875,3540,6127,2581,2642,2573,
            2792,2800,2500,3700,6030,5437,2758,3490,2851,2720)

diff <- attendance - claim
diff

#determine when the attendance was greater than 3000
pos <- length(diff[diff > 0])
pos

#determine when attendance was less than 3000
neg <- length(diff[diff < 0])
neg

#run the sign test
res <- binom.test(x = c(pos,neg), alternative = 'two.sided')
res
res$p.value

#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")

####Lottery Ticket Sales#####

##hypothesis: median = 200

#set significance level
alpha <- 0.05

#set the claim
claim <- 200

#critical value
n <- 40
k <- round(((n-1)/2)-0.95*(sqrt(n)))
cat("Critical value = ", k)

#40 values taken at random of which 15 are less than 200

tickets <- c(621,315,270,153,168,301,487,103,108,354,612,149,168,258,264,257,190,195,279,
             280,250,370,603,543,275,177,105,111,349,285,272,118,129,196,311,267,100,201,
             222,290)
length(tickets)
length(tickets[tickets <200])

diff <- tickets - claim
diff

#determine when the attendance was greater than 3000
pos <- length(diff[diff > 0])
pos

#determine when attendance was less than 3000
neg <- length(diff[diff < 0])
neg

#run the sign test
res <- binom.test(x = c(pos,neg), alternative = 'less')
res
res$p.value

#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")


#######wilcoxon rank sum test#######
###length of prison sentences###

##significance value
alpha <- 0.05

#vector of values
male <- c(8,12,6,14,22,27,3,2,2,2,4,6,19,15,13)
female <- c(7,5,2,3,21,26,3,9,4,0,17,23,12,11,16)

#critical value
Critical_value <- qwilcox(m=15,n=15,p=0.05)
cat("Critical value = ", Critical_value)

#testing the result
res <- wilcox.test(x= male, y= female, alternative = 'two.sided', correct = 'FALSE')
res
res$p.value


#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")


###winning baseball games

##significance value
alpha <- 0.05

#vector of values
NL <- c(89,9,8,101,90,91,9,96,108,100,9,6,8,2,5)
AL <- c(108,8,9,97,100,102,9,104,95,89,8,101,6,1,5,8)

#critical value
Critical_value <- qwilcox(m=15,n=16,p=0.05)
cat("Critical value = ", Critical_value)

#testing the result
res <- wilcox.test(x= NL, y= AL, alternative = 'two.sided', correct = 'FALSE')
res
res$p.value


#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")



#kruskal-wallis test#
###mathematics literacy scores

##significance value
alpha <- 0.05

GroupA <- data.frame(maths = c(527,406,474,381,411), group = rep('GroupA',5))
GroupB<- data.frame(maths = c(520,510,513,548,496),group = rep('GroupB',5))
GroupC <- data.frame(maths = c(523,547,547,391,549), group = rep('GroupC',5))

##combine dataframes into one
data <- rbind(GroupA,GroupB,GroupC)

#critical value
ls_Critical_value <- qchisq(p = 0.05, df = 14)
cat("Computed critical value = ", ls_Critical_value)

#run test
res <- kruskal.test(maths ~ group, data = data)
res
res$p.value

#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")



####spearman rank coefficient
##subway and commuter rail passengers

#set significance value
alpha <- 0.05

#create vectors

City <- c(1,2,3,4,5,6)
Subway <- c(845,494,425,313,108,41)
Rail <-	c(39,291,142,103,33,38)

#combine into one dataframe
data <- data.frame(city = City, subway = Subway, rail = Rail)

#test
res <- cor.test(data$subway, data$rail, method = 'spearman')
res
res$p.value
res$estimate


#critical value
n <- length(data)
passengers_data <- sqrt((n-2)/(1-(res$estimate*res$estimate)))
cat("critical value = ", passengers_data)

#determine the result
ifelse(res$p.value > alpha, "Fail to reject the null", "reject the null")



####caramel boxes
#Possible outcomes
sample <- 40
prize <- 4
prob_mas <- 1/4
lambda <- (1/prize)
lambda 
sample_mean = rep(NA, sample)

#determining probability
pop_mean<- 1/lambda
Avg_mean<- mean(sample_mean)
cat("Population mean = ", pop_mean, " Averaged sample mean =  ", Avg_mean, "\n")


n = 40 
ncol = 1
simdata <-matrix(list(), nrow=n, ncol=1)
simdata

stdev   <- sqrt(((1/lambda)^2)/sample) 
stdev

num = 40 
newdata <- matrix(list() , nrow = num, ncol =1)
wins <- vector()
for(j in 1:num){
  data <- vector()
  a=0
  b=0
  c=0
  d=0
  while(a*b*c*d==0){
    x<- sample(1:4,1)
    if(x==1){
      a=1
      data <- append(data,"a")
    } else if (x==2){
      b=1
      data <- append(data,"b")
    } else if (x==3){
      c=1
      data <- append(data,"c")
    } else {
      d=1
      data <- append(data,"d")
    }
  }
  simdata[[j,1]] <- data
  wins <- append(wins,length(data))
}
sprintf("Total number of boxes bought for %d wins is %d ",num, sum(wins))

sprintf("The average number of boxes a person must buy to get all four prizes is %d "
        , ceiling(sum(wins)/num))

datatable<-as.table(simdata)
datatable





samples <- 30
data_points <- 3
b = 60/100
b
i = 30/100
i
g = 10/100
g
lambda <- (1/data_points)
cat("Lambda value is = ", lambda)
sample_means = c()


#Determine the probability of each outcome</i>
number =30
Simulationdata <-matrix(list(), nrow=number, ncol=1)
Selected <- vector()
set.seed(123)
for(j in 1:number){
  data <- vector()
  b=0; i=0; g=0
  while(b*i*g==0){
    dat<- sample(0:9,1)
    if(dat<=5){
      b=1
      data <- append(data,"b")
    } else if (dat<=8){
      i=1
      data <- append(data,"i")
    } else {
      g=1
      data <- append(data,"g")
    }
  }
  Simulationdata[[j,1]] <- data
  Selected <- append(Selected,length(data))
}
cat("Total number of lottery brought", sum(Selected) ," and Average number of tickets a person must buy" ,sum(Selected)/number )



###lottery winner####
sample <- 30
dp <- 3
b = 60/100 #letter b
b
i = 30/100 #letter i
i
g = 10/100 #letter g
g
lambda <- (1/dp)
cat("Lambda value is = ", lambda)
sample_means = c()


#Determine the probability of each outcome</i>
num =30
sd <-matrix(list(), nrow=num, ncol=1)
sel <- vector()
set.seed(123)
for(j in 1:num){
  data <- vector()
  b=0; i=0; g=0
  while(b*i*g==0){
    dat<- sample(0:9,1)
    if(dat<=5){
      b=1
      data <- append(data,"b")
    } else if (dat<=8){
      i=1
      data <- append(data,"i")
    } else {
      g=1
      data <- append(data,"g")
    }
  }
  sd[[j,1]] <- data
  sel <- append(sel,length(data))
}
cat("Total number of lottery brought", sum(sel) ," and Average number of tickets a person must buy" ,sum(sel)/num)


