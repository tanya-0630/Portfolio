print("Plotting Basics: Tanya Fitkiriwala")

#install packages
install.packages("plyr")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")

#call packages
library("plyr")
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("ggplot2")
library("moments")

#loading the data
data("BullTroutRML2")

summary(BullTroutRML2$age)
sd(BullTroutRML2$age)
var(BullTroutRML2$age)

x <- BullTroutRML2$age
y <- dnorm(x, mean = 5.771, sd = 2.925)
plot(x,y, pch = 8, xlab = "Fork Length(mm)", ylab = "Probability Density")

summary(BullTroutRML2$fl)
sd(BullTroutRML2$fl)
var(BullTroutRML2$fl)

x <- BullTroutRML2$fl
y <- dnorm(x, mean = 326.1, sd = 112.2022)
plot(x,y, pch = 8, xlab = "Fork Length(mm)", ylab = "Probability Density")

summary(BullTroutRML2$lake)

x <- c(levels(BullTroutRML2$lake))
y <- c(61,35)
pie(y,labels = x, col = rainbow(length(y)))


summary(BullTroutRML2$era)

x <- c(levels(BullTroutRML2$era))
y <- c(38,58)
pie(y,labels = x, col = rainbow(length(y)))
                       


                       #print first three and last three rows
head(BullTroutRML2, n=3)
tail(BullTroutRML2, n=3)

#filter harrison lake records
flt <- filter(BullTroutRML2, lake== "Harrison")
str(flt)
t <- summary(flt)
t

plot(age~fl, data = flt, xlim=c(0,500), ylim=c(0,15), main = "Plot 1: Harrison Lake Trout",
     ylab = "Age(Yrs)", xlab = "Fork Length(mm)", pch = 16, cex =2)

hist(flt$age, ylab = "Frequency", xlab = "Age(Yrs)", 
  main = "Plot 2: Harrison Fish Age Distribution", col = "cadetblue", col.main = "cadetblue")

flt2 <- filter(BullTroutRML2, lake== "Osprey")

hist(flt2$age, ylab = "Frequency", xlab = "Age(Yrs)", 
     main = "Plot 2.1: Osprey Fish Age Distribution", col = "navyblue", col.main = "navyblue", cex = 1)


level <- as.factor(flt$era)
level
cols <- c("gray0","blue")
plot(age~fl, data = flt, xlim=c(0,500), ylim=c(0,15), main = "Plot 3: Harrison Density Shaded by Era",
     ylab = "Age(Yrs)", xlab = "Fork Length(mm)", pch = 16, col = cols[level], las = 2, cex = 2)
legend("topleft",inset=0.05, legend = levels(level), pch = 16, col = c("gray0","blue"), bty = "o")


tmp <- headtail(BullTroutRML2, n=3)
tmp$era

pchs <- c("+","x")
cols <- c("red", "gray60")

sapply(tmp, class)
tmp

tmp1 <- lapply(tmp, function(x) as.numeric(as.numeric(x)))
tmp1
numera <- as.numeric(tmp1$era)
numera

plot(age~fl, data = BullTroutRML2, xlim=c(0,500), ylim=c(0,15), main = "Plot 4: Symbol & Color by Era",
     ylab = "Age(Yrs)", xlab = "Fork Length(mm)", pch = pchs[numera], col = cols[numera], las = 2, cex = 2)

regline <- lm(age~fl, data = BullTroutRML2)
regline
abline(regline, col = "cadetblue", lty=2, lwd=2)

legend("topleft",inset=0.05, legend = c(1,2), pch = pchs, col = c("red","gray60"), bty = "n")


