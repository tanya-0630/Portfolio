print("Tanya Fitkiriwala")
install.packages("vcd")
library("vcd")
salesdata <- c(8,11,15,20,21,11,18,10,6,22)
temperaturedata <- c(69,80,77,84,80,77,87,70,65,90)
plot(salesdata,temperaturedata)
mean(temperaturedata)
salesdata <- salesdata[-3]
salesdata
salesdata1 <- c(salesdata[3:9])
salesdata <- c(salesdata[1:2],16,salesdata1[1:7])
salesdata
names <- c("Tom","Dick","Harry")
print(names)
y <- matrix(1:10, nrow = 5, ncol = 2)
y
icSales <- data.frame(salesdata,temperaturedata)
icSales
summary(icSales)
filedata <- read.csv("C:/Users/tanya/Downloads/Student.csv",header = TRUE,quote = "")
names(filedata)
