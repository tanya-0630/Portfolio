install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("lubridate")
install.packages("treemapify")
install.packages("RColorBrewer")
install.packages("rlang")
install.packages("ggcorrplot")
install.packages("ggpubr")
install.packages("vctrs")


library(RColorBrewer)
library(janitor)
library(skimr)
library(lubridate)
library(treemapify)
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(rlang)
library(ggplot2)
library(ggcorrplot)
library(vctrs)
library(ggpubr)
library(psych)


#load data
getwd()
setwd("/Users/katherinebhogal/Downloads")
ss=read.csv("Superstore.csv")

head(ss, n=10)
summary(ss)

glimpse(ss)


colnames(ss)
#Cleaning variable names
ss <- clean_names(ss)
colnames(ss)

sapply(ss, class)

#Checking NA values
sum(is.na(ss))
which(is.na(ss), arr.ind=TRUE)

#Removing unwanted variables
su = subset(ss, select = -c(row_id,customer_id,order_id,postal_code,customer_name,product_id) )
colnames(su)

#Adding a variable for profit percent
su <- su %>% 
  mutate(profitpercent = ((profit/(sales-profit)) *100))

colnames(su)

#Changing data type of order dates and extracting year
su$order_date <- as.Date(su$order_date, format = "%m/%d/%Y")
su <- su %>% 
  mutate(order_year = year(order_date))

#Changing data type of order dates and extracting month
su$order_date <- as.Date(su$order_date, format = "%m/%d/%Y")
su <- su %>% 
  mutate(order_month = month(order_date))

#creating seasonality
su$seasonality <- ifelse((su$order_month == 12 | su$order_month == 1 | su$order_month == 2),'Winter',
                         ifelse((su$order_month == 3 | su$order_month == 4 | su$order_month ==5), 'Spring',
                          ifelse((su$order_month == 6 | su$order_month == 7 | su$order_month ==8),'summer','Autumn')))

#calculate time to delivery
su$timetodelivery <- as.Date(su$ship_date, format = "%m/%d/%Y") - as.Date(su$order_date, format = "%m/%d/%Y")

glimpse(su)

#changing data types for categorical variables

su$category <- as.factor(su$category)
su$sub_category <- as.factor(su$sub_category)
su$seasonality <- as.factor(su$seasonality)
su$order_year <- as.factor(su$order_year)


#Descriptive Stats for Cleaned Dataset
describe(su,skew = FALSE, omit = TRUE)


#Boxplot of Profit Percent By Category
ggplot(su, aes(x=category, y=profitpercent,fill=category)) + 
  geom_boxplot()+ theme_bw()+
  labs(title = "Boxplot of Profit Percent By Category")+
  scale_fill_manual(values = c("Furniture" = "yellow",
                               "Office Supplies" = "orange",
                               "Technology" = "violet")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
  

unique(su$category)

# Number of products in each category
su %>% 
  group_by(category) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))

#Count of Categories
cat <- data.frame(table(su$category))
ggplot(su, aes(x=category, fill=category)) +
  geom_bar()+ theme_bw()+ coord_flip() +
  labs(title = "Count of Categories")+
  scale_fill_manual(values = c("Furniture" = "yellow",
                               "Office Supplies" = "orange",
                               "Technology" = "violet")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

n_distinct(su$sub_category)

# category and sub category distribution
#ggplot(su, aes(x= sub_category, y = category, fill=category)) + 
#  geom_col(position = position_dodge(),show.legend = TRUE)+ coord_flip()+ 
#  theme_classic() + labs(title = "Sub-category distribution by Category")+
#  scale_fill_manual(values = c("Furniture" = "yellow",
#                               "Office Supplies" = "orange",
#                               "Technology" = "violet")) +
#  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

#sub category count by category
ggplot(su, aes(x= sub_category, fill = category)) + 
  geom_bar() + theme_bw() + coord_flip() + 
  labs(title = "Count of Sub Category",
       x = "Sub_Category", y = "Frequency")+
  scale_fill_manual(values = c("Furniture" = "yellow",
                               "Office Supplies" = "orange",
                               "Technology" = "violet")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))



colnames(su)
su$order_year

# yearly profits by category
su %>% 
  group_by(category, order_year) %>% 
  dplyr::summarize(total_profit=sum(profit)) %>% 
  ggplot(aes(x= order_year, y=total_profit, color = category)) + 
  geom_point(size = 6, alpha=0.5 ) + geom_line(size=1.5) +
  theme_bw() +
  labs(title = "Yearly trend of Profits by Category",
       x= "Year", y = "Profit (dollars)")+
  scale_fill_manual(values = c("Furniture" = "#E9BFDD",
                               "Office Supplies" = "blue",
                               "Technology" = "violet"))+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))



#Top 5 product distribution

su %>% 
  group_by(product_name) %>% 
  dplyr::summarize(count_of_products=n()) %>% 
  arrange(desc(count_of_products)) %>% 
  slice_head(n=5) %>% 
  ggplot(su, 
         mapping = aes(x= product_name,
                       y= count_of_products))+
  geom_segment(aes(x = reorder(product_name, count_of_products), xend = product_name,
                   y = 0, yend = count_of_products), size =1,)+
  geom_point(size=10, color= "red",
             shape = 15, stroke = 3) + coord_flip() + theme_bw() +
  labs(title = "Distribution of Top 5 Products",
       x = NULL) +
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5)) +
  geom_text(aes(label= count_of_products), 
            color="black", size=4, vjust=0.5, fontface='bold')


#su %>% 
#  group_by(customer_name) %>% 
#  dplyr::summarise(top_10_customer=n()) %>% 
#  arrange(desc(top_10_customer)) %>% 
#  slice_head(n=10)


# The most used mode of shipment
#su %>% 
#  group_by(ship_mode, segment) %>% 
#  dplyr::summarise(total=n()) %>% 
#  ggplot(aes(x=ship_mode, y = total, fill= ship_mode)) + 
#  geom_col(position=position_dodge()) + facet_wrap(~segment)+ theme_bw()+
#  theme(axis.text.x = element_text(angle = 60))+
#  labs(title = "Distribution by Ship Mode and Segment") +
#  scale_fill_manual(values = c("First Class" = "skyblue",
#                               "Same Day" = "magenta",
#                               "Second Class" = "violet",
#                               "Standard Class" = "Pink")) +
#  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
#  geom_text(aes(ship_mode, label = total),vjust = -0.4)

  
unique(su$sub_category)

# Category sales by region
su %>% 
  ggplot(aes(x=category, fill= region)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Category Product Sales by Region")+
  theme(plot.title = element_text(size=15, face="bold",hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0))
  


# sales distribution by region

su %>% 
  group_by(region) %>% 
  dplyr::summarize(total_sales = round(sum(sales))) %>% 
  ggplot(aes(area = total_sales, fill = region, 
             label= paste0(region, "\n",prettyNum(total_sales, ",")))) +
  geom_treemap() + geom_treemap_text(color= "white",
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Sales Distribution by Region") + 
  scale_fill_brewer(palette="Spectral")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


#Profit distribution by region
su %>% 
  group_by(region) %>% 
  dplyr::summarize(total_profit = round(sum(profit))) %>% 
  ggplot(aes(area = total_profit, fill = region, 
             label= paste0(region, "\n",prettyNum(total_profit, ",")))) +
  geom_treemap() + geom_treemap_text(color= "white",
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Profit Distribution by Region") + 
  scale_fill_brewer(palette="Spectral")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

#Profit distribution by State
su %>% 
  group_by(state) %>% 
  dplyr::summarize(total_profit = round(sum(profit))) %>% 
  ggplot(aes(area = total_profit, fill = state, 
             label= paste0(state, "\n",prettyNum(total_profit, ",")))) +
  geom_treemap() + geom_treemap_text(color= "white",
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Profit Distribution by State") + 
  theme(plot.title = element_text(size=20,face="bold", hjust = 0.5))



#install.packages("gmodels")
#library(gmodels)

#comparing categorical values
#CrossTable(su$ship_mode, su$category, expected = TRUE)

#correlation test between two numerical variables
cor.test(su$sales, su$quantity)



#Correlation Plot 
sv = subset(su, select = c(sales,quantity,discount,profit,profitpercent) )
cor <- cor(sv)
ggcorrplot(cor, colors = c("red","white","royalblue"),
           lab=TRUE)+
  labs(title = "Correlation Between Numerical Variables") + 
  theme(plot.title = element_text(size=20, hjust = 0.5))


# Profit Percent vs. Discount Percent
ggplot(su,aes(x = discount*100, y = profitpercent)) + 
  geom_point() + theme_bw()+
  geom_smooth(method = "lm", se=FALSE) +
  labs(title = "Profit Percent vs Discount Percent") + 
  xlab("Discount Percent")+ ylab("Profit Percent")+
  theme(plot.title = element_text(size=20, hjust = 0.5))+
  stat_regline_equation(label.y = 125, label.x=60,aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 105, label.x=60,aes(label = ..rr.label..))




##### Q1
##### Regression


library(caret)
library(Metrics)

sv = subset(su, select = -c(order_date,ship_date,country,product_name,city) )
str(sv)

sv$ship_mode <- as.factor(sv$ship_mode)
sv$segment <- as.factor(sv$segment)
sv$state <- as.factor(sv$state)
sv$region <- as.factor(sv$region)

colnames(sv)

set.seed(3456)
trainIndex <- createDataPartition(sv$profitpercent, p=0.7, list=FALSE)
train_reg1 <- sv[ trainIndex,]
test_reg1 <- sv[-trainIndex,]

str(train_reg1)

# REGRESSION WITH ALL FEATURES
reg1 <- lm(profitpercent~., data=train_reg1)
summary(reg1)


#Train set prediction
train_reg_pred1 <- predict(reg1, train_reg1)
train_reg_rmse1 <- rmse(train_reg1$profitpercent, train_reg_pred1)
train_reg_rmse1

#Test set prediction
test_reg_pred1 <- predict(reg1, test_reg1)
test_reg_rmse1<- rmse(test_reg1$profitpercent, test_reg_pred1)
test_reg_rmse1


# STEPWISE FEATURE SELECTION 
reg2 <- step(lm(profitpercent~., data=train_reg1), direction = 'both')
summary(reg2)

#Train set prediction
train_reg_pred2 <- predict(reg2, train_reg1)
train_reg_rmse2 <- rmse(train_reg1$profitpercent, train_reg_pred2)
train_reg_rmse2

#Test set prediction
test_reg_pred2 <- predict(reg2, test_reg1)
test_reg_rmse2<- rmse(test_reg1$profitpercent, test_reg_pred2)
test_reg_rmse2


#LASSO REGULARIZATION
str(sv)
library(glmnet)

train.x <- model.matrix(profitpercent~.,train_reg1)[,-11]
test.x <- model.matrix(profitpercent~.,test_reg1)[,-11]

train.y <- train_reg1$profitpercent
test.y <- test_reg1$profitpercent

cv_lasso <- cv.glmnet(train.x, train.y, nfolds = 10)
plot(cv_lasso)
cv_lasso$lambda.1se

reg3 <- glmnet(train.x,train.y,alpha=1,lambda=cv_lasso$lambda.1se)
coef(reg3)

#Train set prediction
train_reg_pred3 <- predict(reg3, train.x)
train_reg_rmse3 <- rmse(train.y, train_reg_pred3)
train_reg_rmse3

#Test set prediction
test_reg_pred3 <- predict(reg3, test.x)
test_reg_rmse3<- rmse(test.y, test_reg_pred3)
test_reg_rmse3

#Rsquared
sst <- sum((train.y - mean(train.y))^2)
sse <- sum((train_reg_pred3- train.y)^2)
R2 <- 1 - sse/sst
R2





######decision tree#######

#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart')
install.packages('rattle')
install.packages('readxl')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rattle)
library(rpart.plot)
library(pROC)



#data splicing
set.seed(12345)
train <- sample(1:nrow(su),size = ceiling(0.80*nrow(su)),replace = FALSE)
train
# training set
su_train <- su[train,]
# test set
su_test <- su[-train,]

str(su_train)
str(su_test)
summary (su_train)

#analyzing the  variable
number.perfect.splits <- apply(X=su[-8], MARGIN = 2, FUN =
                                 function(col){
                                   t <- table(su$region,col)
                                   sum(t == 0)
                                 })

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
number.perfect.splits


# penalty matrix

penalty.matrix <- matrix(c(0, 1, 1, 1,   # cost of predicting A when actual is A, B, C, or D
                           1, 0, 1, 1,   # cost of predicting B when actual is A, B, C, or D
                           1, 1, 0, 1,   # cost of predicting C when actual is A, B, C, or D
                           1, 1, 1, 0),  # cost of predicting D when actual is A, B, C, or D
                         byrow = TRUE, nrow = 4)

penalty.matrix


# building the classification tree with rpart

tree <- rpart(region~discount,
              data=su_train,
              parms = list(loss=penalty.matrix),
              method = "class")

rpart.plot(tree, nn=TRUE)

# choosing the best complexity parameter "cp" 
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# tree prunning using the best cp
tree <- prune(tree, cp=cp.optim)

#Testing the model
pred <- predict(object=tree,su_test[-8],type="class")
pred

#Calculating accuracy
t <- table(su_test$region,pred) 
confusionMatrix(t)

#ROC Curve
p1 <- predict(tree, su_test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(su_test$region, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         main= 'ROC Curve')


##### Clustering

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)

#read file
#su <- read.csv("C:/Users/tanya/OneDrive/ProjectsSubmitted/ALY 6040/GroupProject_SuperstoreDataset.csv")

str(su)
# Select relevant variables for clustering
cls_vars <- su[, c("sales", "profit")]

# Standardize the variables
scaling_vars <- scale(cls_vars)

# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k) {
  kmeans(scaling_vars, centers = k, nstart = 10)$tot.withinss
})

# Plot the within-cluster sum of squares
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters", ylab = "Within-cluster sum of squares")

# Perform k-means clustering with the chosen number of clusters
k <- 3  # Change this value based on the elbow plot
kmeans_results <- kmeans(scaling_vars, centers = k, nstart = 10)

# Assign cluster labels to the original dataset
su$cluster <- as.factor(kmeans_results$cluster)

# Visualize the clusters
library(ggplot2)
ggplot(su, aes(x = sales, y = profit, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis - Superstore Dataset")

# View cluster summaries
library(dplyr)
cluster_summary <- su %>%
  group_by(cluster) %>%
  dplyr::summarise(
    avg_sales = mean(sales),
    avg_profit = mean(profit),
    count = n()
  )
print(cluster_summary)



