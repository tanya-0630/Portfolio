print("Tanya Fitkiriwala")
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("readxl")
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
library(readxl)


getwd()
ss <- read_excel("C:/Users/tanya/Downloads/WalmartUSADataset.xlsx")
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
st <- filter(ss, postal_code != "NA",category != "NA", sub_category != "NA")
st

su = subset(st, select = -c(row_id,customer_id) )
colnames(su)

su <- su %>% 
  mutate(costPrice = sales - profit)

su <- su %>% 
  mutate(profitpercent = (profit/costPrice *100))
colnames(su)


unique(ss$category)

su %>% 
  group_by(category) %>% 
  dplyr::summarise(total= n()) %>% 
  arrange(desc(total))


# Number of products in each category
su %>% 
  group_by(category) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))

n_distinct(ss$sub_category)

# category and sub category distribution
ggplot(su, aes(x= sub_category, y = category, fill=category)) + 
  geom_col(position = position_dodge(),show.legend = TRUE)+ coord_flip()+ 
  theme_classic() + labs(title = "Sub-category distribution by Category")+
  scale_fill_manual(values = c("Furniture" = "red",
                               "Office Supplies" = "blue",
                               "Technology" = "lightgreen")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


su <- su %>% 
  mutate(order_year = year(order_date))

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
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))



# Total Customer Count
n_distinct(su$customer_name)

su %>% 
  group_by(customer_name) %>% 
  dplyr::summarise(top_10_customer=n()) %>% 
  arrange(desc(top_10_customer)) %>% 
  slice_head(n=10)

# The most used mode of shipment

su %>% 
  group_by(ship_mode, segment) %>% 
  dplyr::summarise(total=n()) %>% 
  ggplot(aes(x=ship_mode, y = total, fill= ship_mode)) + 
  geom_col(position=position_dodge()) + facet_wrap(~segment)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 60))+
  labs(title = "Distribution by Ship Mode and Segment") +
  scale_fill_manual(values = c("First Class" = "red",
                               "Same Day" = "green",
                               "Second Class" = "yellow",
                               "Standard Class" = "cyan")) +
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))+
  geom_text(aes(ship_mode, label = total),vjust = -0.4)

  

sv <- filter(su, order_year == 2014, region == 'West', category == 'Furniture', sub_category == 'Chairs')
glimpse(sv)
shapiro.test(sv$profit)
qqnorm(sv$profit)
qqline(sv$profit)


x <- t.test(sv$profit, mu=50, alternative = 'less', conf.level = 0.95)
x


a <- filter(su, order_year == 2016, region == 'West')
b <- filter(su, order_year == 2016, region == 'South')

t.test(a$sales,b$sales , alternative = 'two.sided',var.equal= TRUE, conf.level=0.95)




f<- filter(su, category == 'Office Supplies')
g<- filter(su, category == 'Furniture')
h<- filter(su, category == 'Technology')

t.test(f$profit,g$profit, alternative = 'two.sided', conf.level=0.95)






