print("Tanya Fitkiriwala")


#Blood Types

#         Expected | Observed   
#Type A     20%     |   12
#Type B     28%     |   8
#Type O     36%     |   24
#Type AB    16%     |   6

#setting significance level
alpha <- 0.05

#critical value
cv <- qchisq(0.05, 3, lower.tail = FALSE)
cv

#observed values vector
obs <- c(12,8,24,6)

#probabilities vector
prob <- c(0.20,0.28,0.36,0.16)
result <- chisq.test(x = obs, p= prob)
result


#On-time performance by airlines
alpha <- 0.05

#critical value
cv <- qchisq(0.05, 3, lower.tail = FALSE)
cv

#observed and prob vector
obs <-  c(125,10,25,40)
 
prob <- c(0.708,0.082,0.09,0.12)

result <- chisq.test(x = obs, p= prob)
result


#chi square independence test

#ethnicity and movie admissions
alpha <- 0.05

#critical value
cv <- qchisq(0.05, 3, lower.tail = FALSE)
cv

#vector
a1 <- c(724,335,174,107)
a2<- c(370,292,152,140)


#matrix
mt = matrix(c(a1,a2), nrow = 2, byrow = TRUE)
mt

#renaming row names and column names
colnames(mt) = c("Caucasian","Hispanic", "African American", "Other")
rownames(mt) = c('2013', '2014')

#result
result <- chisq.test(mt)
result

# women in military
alpha <- 0.05


#critical value
cv <- qchisq(0.05, 3, lower.tail = FALSE)
cv

mt <- matrix(c(c(10791,62491),c(7816,42750),c(932,9525),c(11819,54344)), 
             nrow = 4, byrow = TRUE )
mt
colnames(mt) = c("Officers", "Enlisted")
rownames(mt) = c("Army","Navy","Marine Corps","Air Force")


#result
result <- chisq.test(mt)
result


##one-way anova

#sodium content

#significance level
alpha <- 0.05

#critical value

qf(p = 0.05, df1 =2, df2 = 19, lower.tail = FALSE)

#creating data frames
condiments <- data.frame('sodium' = c(270,130,230,180,80,70,200), 
                         'food' = rep('condiments',7), stringsAsFactors = FALSE)

cereals <- data.frame('sodium' = c(260,220,290,290,200,320,140), 
                         'food' = rep('cereals',7), stringsAsFactors = FALSE)

desserts <- data.frame('sodium' = c(100,180,250,250,300,360,300,160), 
                         'food' = rep('desserts',8), stringsAsFactors = FALSE)

#combining dataframes into one
sodium <- rbind(condiments,cereals,desserts)
sodium$food <- as.factor(sodium$food)

#anova test
anova <- aov(sodium ~ food, data = sodium)
anova
summary(anova)


a.summary <- summary(anova)


##leading companies

alpha <- 0.01

#critical value

qf(p = 0.01, df1 =2, df2 = 11, lower.tail = FALSE)

#creating data frames
cereal <- data.frame('sales' = c(578,320,264,249,237), 
                         'company' = rep('cereal',5), stringsAsFactors = FALSE)

candy <- data.frame('sales' = c(311,106,109,125,173), 
                      'company' = rep('candy',5), stringsAsFactors = FALSE)

coffee <- data.frame('sales' = c(261,185,302,689), 
                       'company' = rep('coffee',4), stringsAsFactors = FALSE)

#combining dataframes into one
sales <- rbind(cereal,candy,coffee)
sales$company <- as.factor(sales$company)

#anova test
anova <- aov(sales ~ company, data = sales)
anova
summary(anova)

a.summary <- summary(anova)

##per pupil expenditure
alpha <- 0.05

#critical value

qf(p = 0.05, df1 =2, df2 = 10, lower.tail = FALSE)

#creating data frames
eastern <- data.frame('expenditure' = c(4946,5953,6202,7243,6113),
                     'states' = rep('eastern',5), stringsAsFactors = FALSE)

middle <- data.frame('expenditure' = c(6149,7451,6000,6479), 
                    'states' = rep('middle',4), stringsAsFactors = FALSE)

western <- data.frame('expenditure' = c(5282,8605,6528,6911), 
                     'states' = rep('western',4), stringsAsFactors = FALSE)

#combining dataframes into one
expenditure <- rbind(eastern,middle,western)
expenditure$states <- as.factor(expenditure$states)

#anova test
anova <- aov(expenditure ~ states, data = expenditure)
anova
summary(anova)

a.summary <- summary(anova)

##Increasing plant growth
alpha <- 0.05

#critical value

qf(p = 0.05, df1 =2, df2 = 10, lower.tail = FALSE)

#creating data frames
A <- data.frame('plantfood' = c(9.2, 9.4, 8.9),
                      'light' = rep('Light 1',3), 'food' = rep('Food A', 3),stringsAsFactors = FALSE)

B <- data.frame('plantfood' = c(7.1, 7.2, 8.5),
                'light' = rep('Light 1',3), 'food' = rep('Food B', 3),stringsAsFactors = FALSE)

C <- data.frame('plantfood' = c(8.5, 9.2, 8.9),
                'light' = rep('Light 2',3), 'food' = rep('Food A', 3),stringsAsFactors = FALSE)

D <- data.frame('plantfood' = c(5.5, 5.8, 7.6),
                'light' = rep('Light 2',3), 'food' = rep('Food B', 3),stringsAsFactors = FALSE)


food <- rbind(A,B,C,D)

#two way anova test

anova2 <- aov(plantfood ~ light + food, data = food)
summary(anova2)

anova3 <- aov(plantfood ~ light * food, data = food)
summary(anova3)


#baseball data
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("car")
install.packages("janitor")
install.packages("tidyr")
install.packages("leaps")
install.packages("stargazer")

library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(janitor)
library(tidyr)
library(leaps)
library(stargazer)

base <- read.csv('C:/Users/tanya/Downloads/baseball-1.csv')
glimpse(base)
unique(base$Team)
unique(base$League)
unique(base$Year)
unique(base$Playoffs)

num_cols <- sapply(base, is.numeric)
base_num <- base[,num_cols] 
base_num[is.na(base_num)] <- 0
str(base)
stargazer(base, type = "text")

#scatter plot
ggplot(base, aes(W, RA)) + geom_jitter(col = 'red') + labs(x = "Wins", y = "Runs Allowed")+ 
  labs(title = "Wins vs Runs Allowed") + 
  theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))


ggplot(base, aes(W, RS)) + geom_jitter(col = 'violet') + labs(x = "Wins", y = "Runs Scored")+ 
  labs(title = "Wins vs Runs Scored") + 
  theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))

#correlation plot
cors <- cor(base_num, use = 'pairwise')
corrplot(cors, type = 'upper', diag = TRUE, t1.cex = 1)

#wins in different leagues
win_y <- aggregate.data.frame(base$W, by=list(Year=base$Year,
                                                     League=base$League),FUN = "sum")
names(win_y) <- c("Year","League", "Wins")
ggplot(win_y, aes(Year,Wins, color = League))+
  labs(title = "Number of wins by Year")+
  theme(plot.title = element_text(size=11, face="bold", hjust = 0.5))+
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  geom_line(stat = "identity", size=1)


  
# number of teams by year
team_y <- base%>%
  group_by(Year) %>%
  dplyr::summarize(total_team=n_distinct(Team))%>%as.data.frame()

  ggplot(team_y, aes(Year, total_team, color = 'red'))+
    labs(title = "Number of teams by Year", x = 'Year', y= 'Total Teams')+
    theme(plot.title = element_text(size=11, face="bold", hjust = 0.5))+
    geom_line(stat = "identity", size = 1.5)

#chi-sqaure test
  
alp <- 0.05

critical_v <- qchisq(0.05, 5, lower.tail = FALSE)
critical_v


base$Decade <- base$Year-(base$Year%%10)

winnings <- base%>%
  group_by(Decade)%>%
  summarise(Total_wins=sum(W))%>%as_tibble() 

base_result <- chisq.test(winnings)
base_result
