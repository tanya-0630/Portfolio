getwd()
system("java -version")

install.packages("rlang")

install.packages("sparklyr")
library(sparklyr)


SPARK_HOME = "C:/Spark/spark-3.4.1-bin-hadoop3/"
sc <- spark_connect(master = "local", spark_home = SPARK_HOME)
spark_connect()

#annual house price index three digits zip
data1 <- spark_read_csv( 
  sc, 
  path = "C:/Users/tanya/OneDrive/ProjectsSubmitted/ALY 6110/HPI_AT_BDL_ZIP3.csv", 
  header = TRUE
) 

head(data2, n = 10)
data2 <- as.data.frame(data2)
summary(data2)

# Identify missing values
missing_values <- is.na(data2$Annual_Change_)


# Remove rows with missing values
data2 <- data2[!missing_values, ]

# Convert to numeric
data2$Annual_Change_ <- as.numeric(as.character(data2$Annual_Change_))

data2$HPI <- as.numeric(as.character(data2$HPI))

data2$HPI_with_1990_base <- as.numeric(as.character(data2$HPI_with_1990_base))

data2$HPI_with_2000_base <- as.numeric(as.character(data2$HPI_with_2000_base))

summary(data2)

library(ggplot2)

dim(data2)

ggplot(data=data2, aes(x=FiveDigit_ZIP_Code)) +
  geom_histogram(fill="violet", color="black") +
  ggtitle("Histogram Distribution of Zip Codes")


ggplot(data=data2, aes(x=FiveDigit_ZIP_Code, y=HPI)) + 
  geom_point()

ggplot(data=data2, aes(x=HPI_with_2000_base, y=HPI)) + 
  geom_boxplot(fill="orange")




