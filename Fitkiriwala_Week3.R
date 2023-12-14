print("Tanya Fitkiriwala")
install.packages('ISLR')
install.packages('caret')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('pROC')
install.packages('dplyr')
install.packages("janitor")
install.packages("tidyr")
install.packages("leaps")
library(ISLR)
library(caret)
library(ggplot2)
library(gridExtra)
library(pROC)
library(dplyr)
library(janitor)
library(tidyr)
library(leaps)

#load data
attach(College)
head(College, n=10)


#exploratory data analysis
summary(College)
glimpse(College)

#Remove only not available rows-completely missing values
data <- College[rowSums(is.na(College)) != ncol(College), ]        # Drop empty rows
data   
nrow(data)
ncol(data)

#Remove columns that has null values
data <- data[ , colSums(is.na(data)) != nrow(data)]       # Drop empty columns
data  
ncol(data)

library(ggplot2)
qplot(Expend,Grad.Rate,data=College,
      main="Expenditure vs Grad Rate by Sector Division",
      xlab="Expenditure (per student)",ylab="Grad Rate", color = Private, shape = Private, geom = 'point')+
  scale_shape(solid = FALSE)

y<- qplot(x= Private, y= Expend, fill = Private, geom = 'boxplot')+
  guides(fill = FALSE)
z<- qplot(x= Private, y= Grad.Rate, fill = Private, geom = 'boxplot')+
  guides(fill = FALSE)
grid.arrange(y,z, nrow = 1)

data_coll <- College

#correlation testing
data_coll$Private <- ifelse(data_coll$Private == 'Yes', 1, 0)
unique(data_coll$Private)

library(corrplot)
cors <- cor(data_coll, use = 'pairwise')
corrplot(cors, type = 'upper', diag = TRUE, t1.cex = 1)


#splitting data into test and train sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(College$Private, p = 0.70, list =  FALSE)
train <- data_coll[trainIndex,]
test <- data_coll[-trainIndex,]

glimpse(train)

##fitting a logistic regression
m1 <- glm(Private ~ ., data = train, family = binomial(link = 'logit'))
summary(m1)

#feature selection
f<- step(lm(Private ~ 1, data = data_coll), direction = 'forward', scope = formula(lm(Private ~ ., data = data_coll)))
f$anova
f$coefficients

m2 <- glm(Private ~ F.Undergrad+Outstate+PhD+S.F.Ratio+Grad.Rate+
            Apps+Terminal+perc.alumni+Room.Board+Accept+Top10perc+Expend,
          data = train, family = binomial(link = 'logit'))
summary(m2)

exp(coef(m2))

#min, mean, and max values
summary(College$Expend)

#create dataset to see the changes in probability
testdata <- data.frame(Private = 'Yes',
  F.Undergrad = c(min(data_coll$F.Undergrad),mean(data_coll$F.Undergrad),max(data_coll$F.Undergrad)),
  Outstate = c(min(data_coll$Outstate),mean(data_coll$Outstate),max(data_coll$Outstate)),
  PhD = c(min(data_coll$PhD),mean(data_coll$PhD),max(data_coll$PhD)),
  S.F.Ratio= c(min(data_coll$S.F.Ratio),mean(data_coll$S.F.Ratio),max(data_coll$S.F.Ratio)),
  Grad.Rate = c(min(data_coll$Grad.Rate),mean(data_coll$Grad.Rate),max(data_coll$Grad.Rate)),
  Apps= c(min(data_coll$Apps),mean(data_coll$Apps),max(data_coll$Apps)),
  Terminal = c(min(data_coll$Terminal),mean(data_coll$Terminal),max(data_coll$Terminal)),
  perc.alumni = c(min(data_coll$perc.alumni),mean(data_coll$perc.alumni),max(data_coll$perc.alumni)),
  Room.Board = c(min(data_coll$Room.Board),mean(data_coll$Room.Board),max(data_coll$Room.Board)),
  Accept = c(min(data_coll$Accept),mean(data_coll$Accept),max(data_coll$Accept)),
  Top10perc = c(min(data_coll$Top10perc),mean(data_coll$Top10perc),max(data_coll$Top10perc)),
  Expend = c(min(data_coll$Expend),mean(data_coll$Expend),max(data_coll$Expend)))
testdata

#predicting probability
testdata$prob <- predict(m2, testdata, type = "response")
testdata


#train set predictions
prob.train <- predict(m2, newdata = train, type = 'response')
predicted.classes.min <- as.factor(ifelse(prob.train >= 0.5, "Yes", "No"))
tail(predicted.classes.min)

train$Private <- ifelse(train$Private == 1, "Yes", "No")
unique(train$Private)
train$Private <- as.factor(train$Private)
unique(College$Private)
glimpse(train)

#model accuracy
confusionMatrix(predicted.classes.min, train$Private, positive = 'Yes')


##test set predictions
prob.test <- predict(m2, newdata = test, type = 'response')
predicted.classes.min <- as.factor(ifelse(prob.test >= 0.5, "Yes", "No"))
tail(predicted.classes.min)


test$Private <- ifelse(test$Private == 1, "Yes", "No")
unique(test$Private)
test$Private <- as.factor(test$Private)
unique(College$Private)
glimpse(test)
confusionMatrix(predicted.classes.min, test$Private, positive = 'Yes')

#ROC Curve
R1 <- roc(test$Private, prob.test)

plot(R1, col = 'violet', ylab = 'Sensitivity- TP Rate', xlab = 'Specificity- FP Rate')
auc <- auc(R1)
auc

