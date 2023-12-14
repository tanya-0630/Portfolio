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
ss <- read_excel("C:/Users/tanya/OneDrive/Projects Submitted/ALY 6040/Superstore_Dataset.xlsx")
head(ss, n=10)
summary(ss)
colnames(ss)

glimpse(ss)

ss <- clean_names(ss)
ss
colnames(ss)
sapply(ss, class)

#check for NA

sum(is.na(ss))
which(is.na(ss), arr.ind=TRUE)
st <- filter(ss, postal_code != "NA",category != "NA", sub_category != "NA")
st

# Check for duplicates
sum(duplicated(st))
st <- st[!duplicated(st), ]
dim(st)


#remove columns
su = subset(st, select = -c(row_id,customer_id) )
colnames(su)

#add more columns
su <- su %>% 
  mutate(costPrice = sales - profit)

su <- su %>% 
  mutate(profitpercent = (profit/costPrice *100))
colnames(su)


unique(ss$category)



# Number of products in each category
su %>% 
  group_by(category) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))


n_distinct(ss$sub_category)

# category and sub category distribution
ggplot(su, aes(x= sub_category, y = category, fill=category)) + 
  geom_col(position = position_dodge(),show.legend = TRUE)+ coord_flip()+ 
  theme_classic() + labs(title = "Distribution of Sub-category by Category")+
  scale_fill_manual(values = c("Furniture" = "brown",
                               "Office Supplies" = "darkgreen",
                               "Technology" = "blue")) +
  theme(plot.title = element_text(size=17, face="bold", hjust = 0.5))

#sub category count by category
ggplot(su, aes(x= sub_category, fill = category)) + 
  geom_bar() + theme_bw() + coord_flip() + 
  labs(title = "Count by Category and Sub_Category",
       x = "Sub_Category", y = "Frequency")+
  scale_fill_manual(values = c("Furniture" = "brown",
                               "Office Supplies" = "darkgreen",
                               "Technology" = "blue")) +
  theme(plot.title = element_text(size=17, face="bold", hjust = 0.5))


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
                   y = 0, yend = count_of_products), size =2,)+
  geom_point(size=10, color= "violet",
             shape = 16, stroke = 2) + coord_flip() + theme_bw() +
  labs(title = "Distribution of Top 5 Products",
       x = NULL) +
  theme(plot.title = element_text(size=16, face="bold", hjust = 0.5)) +
  geom_text(aes(label= count_of_products), 
            color="black", size=4, vjust=0.5, fontface='bold')




# sub category sales by region
su %>% 
  ggplot(aes(x=sub_category, fill= region)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Sub-Category Product Sales by City")+
  theme(plot.title = element_text(size=15, face="bold",hjust = 0.5))


# sales distribution by region

su %>% 
  group_by(region) %>% 
  dplyr::summarize(total_sales = round(sum(sales))) %>% 
  ggplot(aes(area = total_sales, fill = region, 
             label= paste0(region, "\n",prettyNum(total_sales, ",")))) +
  geom_treemap() + geom_treemap_text(color= "beige",
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Sales Distribution by Region") + 
  scale_fill_brewer(palette="Spectral")+
  theme(plot.title = element_text(size=17, face="bold", hjust = 0.5))

#Profit distribution by region
su %>% 
  group_by(region) %>% 
  dplyr::summarize(total_profit = round(sum(profit))) %>% 
  ggplot(aes(area = total_profit, fill = region, 
             label= paste0(region, "\n",prettyNum(total_profit, ",")))) +
  geom_treemap() + geom_treemap_text(color= "beige",
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Profit Distribution by Region") + 
  scale_fill_brewer(palette="Spectral")+
  theme(plot.title = element_text(size=17, face="bold", hjust = 0.5))


colnames(su)
glimpse(ss)


#checking for outliers
plot(su$profit, su$sales, main = 'Profit vs Sales Scatterplot', xlab = 'Profit', ylab = 'Sales')
plot(su$profit, su$discount)













