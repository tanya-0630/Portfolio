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
install.packages("janitor")
install.packages("skimr")
install.packages("lubridate")
install.packages("treemapify")
install.packages("RColorBrewer")
install.packages('BSDA')



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
library(RColorBrewer)
library(janitor)
library(skimr)
library(lubridate)
library(treemapify)
library(BSDA)

getwd()
ss <- read.csv("C:/Users/tanya/Downloads/PropertyListings.csv")
ss <- as.data.frame(ss)

head(ss, n=10)
summary(ss)
colnames(ss)

glimpse(ss)



ss <- clean_names(ss)
ss
colnames(ss)
sapply(ss, class)

sum(is.na(ss))
which(is.na(ss), arr.ind=TRUE)
st <- filter(ss, city != "NA",county != "NA", beds != "NA", baths != 'NA', sqft != 'NA', room_in_apt != 'NA',
)
glimpse(st)


su = subset(st, select = -c(address,title,descr, lat, lon))
colnames(su)
su$beds <- as.integer(su$beds)

glimpse(su)
summary(su)
colnames(su)


# Listings in each county
su %>% 
  group_by(county) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))

#County Division
ggplot(su, aes(x= county, fill = county)) + 
  geom_bar() + theme_bw() +
  labs(title = "County Division",
       x = "county", y = "Frequency")+
  scale_fill_brewer(palette="Spectral")+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

# bed distribution by county
ggplot(su, aes(x= county, y = beds, fill=county)) + 
  geom_col(position = position_dodge(),show.legend = TRUE)+ coord_flip()+
  theme_classic() + labs(title = "Bedrooms available in different county")+
  scale_fill_brewer(palette="colorblind")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 1))

#filtering a county to analyse city data
sv <- filter(su, county == "santa clara")
unique(sv$city)


#jitter plot for Santa Clara
ggplot(sv, aes(beds, baths)) + geom_jitter() + labs(x = "Beds", y = "Baths")+ 
  labs(title = "Beds vs Baths Analysis of Santa Clara") + 
  theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))

#filtering all other counties
sx <- filter(su, county != "santa clara")

#jitter plot for other counties
ggplot(sx, aes(beds, baths)) + geom_jitter(col = "blue") + labs(x = "Beds", y = "Baths")+ 
  labs(title = "Beds vs Baths Analysis- Other Counties") + 
  theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))

ggplot(sx, aes(beds, baths, color = county)) + geom_jitter() + labs(x = "Beds", y = "Baths")+ 
  labs(title = "Beds vs Baths Analysis- Other Counties") + 
  theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=0.5))+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))



#boxplot
boxplot(sv$sqft ~ sv$beds, vertical = TRUE, xlab = "beds", ylab = "sqft",
        main = "Boxplot - Beds by Size(in Square Feet)",
        pch = 19, col = c("gray","violet","pink","blue","green","yellow","orange","red"))

legend("topleft", legend = unique(sv$beds), # Position and title
       fill = c("gray","violet","pink","blue","green","yellow","orange","red"),  # Color
       inset = c(0.03, 0.05), # Modify margins
       bg = "white") # Legend background color


summary(st)

unique(y$county)

# comparing property prices
y <- filter(su,county == c('santa clara','san francisco'), beds ==3)
y
glimpse(y)

y1 <- filter(su, county == 'santa clara', beds ==3)
y2<- filter(su, county == 'san francisco', beds ==3)

qqnorm(y1$price, main = "Normal Q-Q plot Santa Clara")
qqline(y1$price)


qqnorm(y2$price, main = "Normal Q-Q plot San Francisco")
qqline(y2$price)

t.test(y1$price, y2$price, alternative = 'two.sided', var.equal = TRUE, conf.level = 0.95)

boxplot(y1$price, y2$price, names= c("Santa Clara - Property prices($)", "San Francisco - Property Prices($)"),
        pch = 19, col = c("orange","magenta"),
        main = "Boxplot of property prices")

# average beds in san mateo
rx <- filter(su, county== 'san mateo')
glimpse(rx)
qqnorm(rx$price)
qqline(rx$price)

t.test(rx$price, mu= 2500, alternative = 'greater', conf.level = 0.95)

#subset
summary(su)
ax <-su[,c("year","price","beds","baths","sqft","room_in_apt")]

#correlation pairs
r1 <- cor(ax)
corrplot(r1)
pairs(ax, pch = 18, col = "steelblue")

a <- lm(baths~sqft, data = ax)
summary(a)
a$coefficients

plot(ax$baths, ax$sqft, xlab = "Baths", ylab = "baths")
abline(a=a$coefficients[1], b=a$coefficients[2],  col = "Red")

b <- lm(beds~sqft+baths+price, data = ax)
summary(b)
b$coefficients


plot(ax$price, ax$sqft, xlab = "price", ylab = "sqft")
abline(a=b$coefficients[2], b=b$coefficients[4]+b$coefficients[3],  col = "Red")

