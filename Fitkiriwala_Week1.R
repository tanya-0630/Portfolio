print("Tanya Fitkiriwala")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("car")
install.packages("janitor")
install.packages("tidyr")
install.packages("leaps")

library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(janitor)
library(tidyr)
library(leaps)

##import data
x = read.csv("C:/Users/tanya/OneDrive/Projects Submitted/ALY 6015/led.csv")
x <- as.data.frame(x)

#exploratory analysis
glimpse(x)
head(x, n=10)
summary(x)

#cleaning column headers
x <- clean_names(x)
colnames(x)

#NA values
which(is.na(x), arr.ind=TRUE)
# replacing NA values in data frame

y <-  filter(x, lifeexpectancy != "NA", adult_mortality != "NA", alcohol != "NA",
             hepatitis_b != "NA", bmi != "NA", polio!= "NA", totalexpenditure != "NA",
             diphtheria!= "NA", gdp != "NA", population != "NA", thinness1_19years != "NA",
             thinness5_9years != "NA", incomecompositionofresources != "NA", schooling != "NA")
colnames(y)
which(is.na(y), arr.ind=TRUE)
glimpse(y)
#setting status to 0 and 1: 0 = developing and 1 = developed
y$status[y$status=="Developing"]<- 0
y$status[y$status=="Developed"]<- 1
y$status <- as.integer(y$status)

#drop country column from the data
z <-subset(y, select = -c(country))
glimpse(z)


summary(y)
#analysis

#histogram
hist(z$lifeexpectancy, main = "Life Expectancy")
hist(z$adult_mortality, main = "Adult Mortality")

#boxplot
boxplot(z$population, main = "Population")
z <-subset(z, select = -c(population))

#scatterplot
scatterplot(bmi ~ lifeexpectancy, data = x)
pairs(z, pch = 18, col = "steelblue")
scatterplotMatrix(z, spread = FALSE, smoother.args = list(lty=2), main = "Scatterplot Matrix")

#correlation plot
cors <- cor(z, use = 'pairwise')
corrplot(cors, type = 'upper', diag = TRUE, t1.cex = 1)

scatterplot(adult_mortality ~ lifeexpectancy, data = z) #strong negative correlation
scatterplot(measles ~ lifeexpectancy, data = z) #no significant correlation
scatterplot(alcohol ~ lifeexpectancy, data = z) #correlation closest to 0.5


#fit the model
fit <- lm(formula = lifeexpectancy ~ adult_mortality + hiv_aids
          + incomecompositionofresources + schooling, data = z)
summary(fit)
plot(fit)

par(mfrow = c(2,2))
plot(fit)
dev.off()

#components + residuals -- linearity
crPlots(model = fit)

#spread level plot for fit - homoscedasticity
spreadLevelPlot(fit)

#check for multicollinearity
vif(fit)

#unusual observation
outlierTest(model = fit)

#high leverage observation
hat.plot <- function(fit){
  p<- length(coefficients(fit))
  n<- length(fitted(fit))
  plot(hatvalues(fit), main = 'Index Plot of Hat Values')
  abline(h= c(2,3)*p/n, col = 'red', lty =2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

#influential Observations
cutoff <- 4/(nrow(z) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty =2, col = 'red')

#using power transformation
hist(z$adult_mortality, main = "Adult Mortality")
summary(powerTransform(z$adult_mortality))

z$adult_mortality_sqrt <- sqrt(z$adult_mortality)
hist(z$adult_mortality_sqrt)

fit2 <- lm(formula = lifeexpectancy ~ adult_mortality_sqrt + hiv_aids
          + incomecompositionofresources + schooling, data = z)
summary(fit2)
plot(fit2)


par(mfrow = c(2,2))
plot(fit2)
dev.off()

#feature selection

library(MASS)

#stepwise selection
ax <- z %>% dplyr::select(lifeexpectancy, adult_mortality, hiv_aids,incomecompositionofresources, schooling)

#fit the model
fit3 <- lm(formula = lifeexpectancy ~ adult_mortality + hiv_aids
           + incomecompositionofresources + schooling, data = ax)

#backward stepwise selection
stepAIC(fit3, direction = 'backward')

#forward stepwise selection
stepAIC(fit3, direction = 'forward')

#stepwise stepwise selection
stepAIC(fit3, direction = 'both')

#best subset
leaps <- regsubsets(lifeexpectancy ~ adult_mortality + hiv_aids
                    + incomecompositionofresources + schooling, data = ax, nbest = 4)
summary(leaps)
plot(leaps, scale = 'adjr2')
