print("Tanya Fitkiriwala")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("grid")
install.packages("vcd")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("MASS")
install.packages("ggcorrplot")
install.packages("plotrix")
install.packages("moments")
install.packages("knitr")
install.packages("tinyrex")
install.packages("ggpubr")
install.packages("ggiraphExtra")


library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(grid)
library(vcd)
library(ggplot2)
library(corrplot)
library(MASS)
library(ggcorrplot)
library(plotrix)
library(moments)
library(knitr)
library(tinytex)
library(ggpubr)
library(ggiraphExtra)


x <- read.csv("C:/Users/tanya/Downloads/insurance.csv")
x
head(x, n=10)
glimpse(x)
summary(x)

#boxplot
boxplot(x$charges ~ x$sex, vertical = TRUE, xlab = "Sex", ylab = "Charges",
        main = "Boxplot - Insurance premium vs Sex",
        pch = 19, col = c("violet","orange"))


legend("topleft", legend = unique(x$sex), # Position and title
       fill = c("violet","orange"),  # Color
       inset = c(0.005, 0.05), # Modify margins
       bg = "white") # Legend background color


#jitter Plot
ggplot(x, aes(smoker, sex, color = sex)) + geom_jitter() + labs(x = "Smokers", y = "Sex")+ 
  labs(title = "Distribution of smokers and non-smokers by sex") + 
  theme(axis.text.x = element_text(angle=0, vjust=1, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))

#subset
y<- x[,c("age","children","bmi","charges")]
y

#correlation pairs
pairs(y, pch = 18, col = "steelblue")

#regression table
dv <- lm(formula = charges~ bmi+age, data = y)
summary(dv)

x$male = ifelse(x$sex =="male", 1,0)
x$male
x$male = as.numeric(x$male)

x$smoke = ifelse(x$smoker == "yes", 1,0)
x$smoke
x$smoke = as.numeric(x$smoke)

#part 1

z<- subset(x, select = -c(sex,smoker,region))

#regression after adding male dummy variable
du <- lm(formula = charges~ bmi+age+male, data = z)
summary(du)

ggPredict(du,interactive = FALSE)

plot(z$bmi[z$male==1], z$charges[z$male==1], xlab = "Male and Female bmi", ylab = "Male and Female insurance charges")
points(z$bmi[z$male==0], z$charges[z$male==0], col = "blue")

#coefficients of model
du$coefficients

#female
abline(a=du$coefficients[1], b=du$coefficients[2],  col = "Red")

#Male
abline(a=du$coefficients[1]+du$coefficients[4], b=du$coefficients[2], col = "darkgreen")
du$coefficients

#regression after adding male and smoker dummy variable
dn <- lm(formula = charges~ bmi+male+smoke, data = z)
summary(dn)
ggPredict(dn,interactive = FALSE)

#male and smoker plot
plot(z$bmi[z$male==1][z$smoke==1], z$charges[z$male==1][z$smoke==1], xlab = "Male and smoker bmi", ylab = "Male and smoker insurance charges")
points(z$bmi[z$male==1][z$smoke==0], z$charges[z$male==1][z$smoke==0], col = "blue")

#coefficients
dn$coefficients
#male smokers
abline(a=dn$coefficients[1]+dn$coefficients[3]+dn$coefficients[4], b=dn$coefficients[2], col = "darkgreen")
#male non-smokers
abline(a=dn$coefficients[1]+dn$coefficients[3], b=dn$coefficients[2], col = "red")


#female and smoker plot
plot(z$bmi[z$male==0][z$smoke==1], z$charges[z$male==0][z$smoke==1], xlab = "Female and smoker bmi", ylab = "Female and smoker insurance charges")
points(z$bmi[z$male==0][z$smoke==0], z$charges[z$male==0][z$smoke==0], col = "blue")

#coefficients
dn$coefficients
#female smokers
abline(a=dn$coefficients[1]+dn$coefficients[4], b=dn$coefficients[2], col = "darkgreen")
#male non-smokers
abline(a=dn$coefficients[1], b=dn$coefficients[2], col = "red")


# part 2

#subsets

sub1= subset(x, subset = (x$sex=='male'))
sub2 = subset(x, subset = (x$sex=='female'))
sub3 = subset(x, subset = (x$smoker=='yes'))
sub4 = subset(x, subset = (x$smoker=='no'))

#sub1

#regression analysis
reg_line1 = lm(sub1$charges~sub1$bmi, data = sub1)
reg_line1
summary(reg_line1)
reg_line1$coefficients

#male bmi and insurance charges
plot(x=sub1$charges, y=sub1$bmi, col = 'red', xlab = 'Male insurance charges', ylab = 'Male BMI',
     main = "Regression on Male insurance and BMI")


#sub 2
plot(x=sub2$charges, y=sub2$bmi, col = 'violet', xlab = 'Female insurance charges', ylab = 'Female BMI',
     main = "Regression on Female insurance and BMI")

#regression analysis
reg_line2 = lm(sub2$charges~sub2$bmi, data = sub2)
reg_line2
summary(reg_line2)


#sub3
plot(x=sub3$charges, y=sub3$bmi, col = 'coral', xlab = 'Smoker insurance charges', ylab = 'Smoker BMI',
     main = "Regression on Smoker's insurance and BMI")

#regression analysis
reg_line3 = lm(sub3$charges~sub3$bmi, data = sub3)
reg_line3
summary(reg_line3)

#sub4
plot(x=sub4$charges, y=sub4$bmi, col = 'skyblue', xlab = 'Non-Smoker insurance charges', ylab = 'Non-Smoker BMI',
     main = "Regression on Non-Smoker's insurance and BMI")

#regression analysis
reg_line4 = lm(sub4$charges~sub4$bmi, data = sub4)
reg_line4
summary(reg_line4)
