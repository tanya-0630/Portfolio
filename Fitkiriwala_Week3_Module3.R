print("Tanya Fitkiriwala")

install.packages("datasets")
install.packages("caret")
install.packages('e1071')

library("e1071")
library(datasets)
library(caret)
library(pROC)


# Load the iris dataset
data(iris)

summary(iris)

# structure of the data
str(iris)

# number of rows with missing values
nrow(iris) - sum(complete.cases(iris))

ggplot(iris, aes(x = Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Sepal Length by Species")

ggplot(iris, aes(x = Sepal.Width, fill = Species)) + 
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Sepal Width by Species")

# Split the data into training and testing sets
set.seed(123)
train <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[train, ]
testData <- iris[-train, ]

# Fit the logistic regression model
mod <- glm(Species ~ ., data = trainData, family = "log")

# Make predictions on the testing set
predict <- predict(mod, newdata = testData, type = "response")

# Convert predicted probabilities to class labels
predictedClass <- ifelse(predict > 0.5, "versicolor", "setosa")
predictedClass

# Evaluate the model's accuracy on the testing set
accuracy <- mean(predictedClass == testData$Species)
cat("Accuracy:", accuracy)


roc <- roc(testData$Species, predict, levels = c("setosa", "versicolor"))
plot(roc, main = "ROC Curve for Logistic Regression Model on Iris Dataset")

auc <- auc(roc)
cat("AUC:", auc)

aggregate(iris[, 1:4], by = list(iris$Species), FUN = mean)


#clustering analysis

plot(iris)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
legend("topleft", legend=unique(iris$Species), col=1:length(unique(iris$Species)), pch=1)

plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)
legend("topleft", legend=unique(iris$Species), col=1:length(unique(iris$Species)), pch=1)


# Select the columns to use for clustering
clus <- iris[, 1:4]

# Determine the optimal number of clusters using the elbow method
ws <- c()
for (i in 1:10) {
  ws[i] <- sum(kmeans(clus, centers = i)$withinss)
}
plot(1:10, ws, type = "b", xlab = "Number of Clusters",
     ylab = "Within Sum of Squares")

# Apply K-means clustering with the optimal number of clusters
k <- 3
set.seed(123)
iris_cluster_kmeans <- kmeans(clus, centers = k)

# Visualize the clusters
library(cluster)
clusplot(clus, iris_cluster_kmeans$cluster,
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0)
