print("Tanya Fitkiriwala")
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

#city division
sv %>% 
  group_by(city) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))

colourCount = length(unique(sv$city))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


ggplot(sv, aes(x= city, fill = city)) + 
  geom_bar() + theme_bw() +
  labs(title = "City Division",
       x = "City", y = "Frequency")+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
  theme(axis.text.x = element_text(angle=60, vjust=.5, hjust=0.5))+
  scale_fill_manual(values = getPalette(colourCount))


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




# price change in Santa Clara
sv %>% 
  group_by(city, year) %>% 
  dplyr::summarise(total_price = sum(price)) %>%
  ggplot(aes(x= year, y=total_price, color = city)) + 
  geom_point(size = 6, alpha=0.5 ) + geom_line(size=1.5) +
  theme_bw() +
  labs(title = "Property Price variation in Santa Clara",
       x= "Year", y = "Price (dollars)")+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
  scale_fill_brewer(palette="Spectral")


# price change in every county
su %>% 
  group_by(county, year) %>% 
  dplyr::summarise(total_price = sum(price)) %>%
  ggplot(aes(x= year, y=total_price, color = county)) + 
  geom_point(size = 6, alpha=0.5 ) + geom_line(size=1.5) +
  theme_bw() +
  labs(title = "Property Price variation in different Counties",
       x= "Year", y = "Price (dollars)")+
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
  scale_fill_brewer(palette="Spectral")



#boxplot
boxplot(sv$sqft ~ sv$beds, vertical = TRUE, xlab = "beds", ylab = "sqft",
        main = "Boxplot - Beds by Size(in Square Feet)",
        pch = 19, col = c("gray","violet","pink","blue","green","yellow","orange","red"))

legend("topleft", legend = unique(sv$beds), # Position and title
       fill = c("gray","violet","pink","blue","green","yellow","orange","red"),  # Color
       inset = c(0.03, 0.05), # Modify margins
       bg = "white") # Legend background color





















