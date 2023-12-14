print("Tanya Fitkiriwala")

install.packages("mlbench")
install.packages("caret")
install.packages('e1071')

library("e1071")
library(mlbench)
library(caret)
library(pROC)
library(ggplot2)

# Load the data set

data("Glass")

summary(Glass)

# structure of the data
str(Glass)

# number of rows with missing values
nrow(Glass) - sum(complete.cases(Glass))

dim(Glass)

library(dplyr)
library(ggplot2)
library(reshape2)

# Visualize the distribution of the target variable (Type of glass)
ggplot(Glass, aes(x = Type, fill = Type)) + geom_bar()+
  labs(title = "Counts of Glass Types",
       x = "Glass Type",
       y = "Count")

#Create a correlation matrix to identify highly correlated variables
cor_mat <- cor(Glass[,1:9])
cor_df <- as.data.frame(melt(cor_mat))
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()+

  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) 

# Create a correlation matrix to identify highly correlated variables
cor_mat <- cor(Glass[,1:9])
cor_df <- as.data.frame(melt(cor_mat))
cor_df <- cor_df[cor_df$value != 1, ] # Exclude self-correlations
cor_df <- cor_df[order(abs(cor_df$value), decreasing = TRUE), ] # Order by correlation strength
cor_df

plot(Glass$Ca, Glass$RI, col=Glass$Type, xlab="Ca", ylab="RI", main="Scatter plot of Ca vs. RI")

plot(Glass$Mg, Glass$Al, col=Glass$Type, xlab="Mg", ylab="Al", main="Scatter plot of Mg vs. Al")

# Split the data into training and testing sets
set.seed(123)
col <- c("Mg","Al","Type")
Glass.col <- Glass[,col]

train_index <- sample(nrow(Glass.col), nrow(Glass.col) * 0.8)
train_data <- Glass.col[train_index, ]
test_data <- Glass.col[-train_index, ]

# Train the SVM model
svm_model <- svm(Type ~ ., data = train_data, kernel = "linear", cost = 1, scale = FALSE)
print(svm_model)

plot(svm_model, train_data[,col])

#model tuning for optimal cost
tuned <- tune(svm, Type ~ ., data = train_data, kernel = "linear",
                    ranges = list(cost = c(0.1, 1, 10, 100, 1000)))
summary(tuned)

#re fit the svm model
svm_tuned <- svm(Type ~ ., data = train_data, kernel = "linear",
              cost = tuned$best.parameters$cost)



# Make predictions on the testing set
svm_pred <- predict(svm_tuned, test_data, type = "Type")
plot(svm_pred)


# Evaluate the model performance
conf_mat <- table(Predicted = svm_pred, Actual = test_data$Type)
print(conf_mat)
mean(svm_pred== test_data[,3])

svm_pred==test_data[,3]




