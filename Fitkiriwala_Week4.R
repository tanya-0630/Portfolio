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
install.packages('glmnet')
install.packages('Metrics')
library(ISLR)
library(caret)
library(ggplot2)
library(gridExtra)
library(pROC)
library(dplyr)
library(janitor)
library(tidyr)
library(leaps)
library(glmnet)
library(Metrics)

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

glimpse(College)
library(ggplot2)
qplot(Enroll,F.Undergrad,data=College,
      main="Enrolment vs Full-Time Undergraduates by Sector Division",
      xlab="New Enrolments",ylab="Undergraduates", color = Private, shape = Private, geom = 'point')+
  scale_shape(solid = FALSE)

y<- qplot(x= Private, y= Enroll, fill = Private, geom = 'boxplot')+
  guides(fill = FALSE)
z<- qplot(x= Private, y= F.Undergrad, fill = Private, geom = 'boxplot')+
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
trainIndex <- sample(x= nrow(data_coll), size = nrow(data_coll) * 0.7)
train <- data_coll[trainIndex,]
test <- data_coll[-trainIndex,]

glimpse(train)


#training and testing the model post feature selection
train_x <- model.matrix(Private ~., train)[,-1]
test_x <- model.matrix(Private ~., test)[,-1]


train_y <- train$Private
test_y <- test$Private

##lasso##

##find best values of lambda
set.seed(123)
cv.lasso <- cv.glmnet(train_x, train_y, nfolds = 10)
plot(cv.lasso)


##finding the minimum and 1se lambda value
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

#fitting the regression model
# alpha = 1
model_min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)
model_min
coef(model_min)

model_se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)
model_se
coef(model_se)

#predicting the RMSE
#train set
pred.train <- predict(model_se, newx = train_x)
train.rmse <- rmse(train_y, pred.train)

#test set
pred.test <- predict(model_se, newx = test_x)
test.rmse <- rmse(test_y, pred.test)



eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}


eval_results(train_y, pred.train, train)
eval_results(test_y, pred.test, test)

#compare rmse
train.rmse
test.rmse


##RIDGE##

##find best values of lambda
set.seed(123)
cv.ridge <- cv.glmnet(train_x, train_y, nfolds = 10)
plot(cv.ridge)


##finding the minimum and 1se lambda value
log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

#fitting the regression model
# alpha = 0
model_min <- glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.min)
model_min
coef(model_min)

model_se <- glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.1se)
model_se
coef(model_se)

#predicting the RMSE
#train set
pred.train <- predict(model_se, newx = train_x)
train.rmse <- rmse(train_y, pred.train)

#test set
pred.test <- predict(model_se, newx = test_x)
test.rmse <- rmse(test_y, pred.test)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

eval_results(train_y, pred.train, train)
eval_results(test_y, pred.test, test)


#compare rmse
train.rmse
test.rmse



####feature selection
f<- step(lm(Private ~ 1, data = data_coll), direction = 'forward', scope = formula(lm(Private ~ ., data = data_coll)))
f$anova
f$coefficients


m2 <- lm(Private ~ F.Undergrad+Outstate+PhD+S.F.Ratio+Grad.Rate+
            Apps+Terminal+perc.alumni+Room.Board+Accept+Top10perc+Expend,
          data = train)
summary(m2)

exp(coef(m2))
