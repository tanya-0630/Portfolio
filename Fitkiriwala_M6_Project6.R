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
ss <- read_excel("C:/Users/tanya/Downloads/Sample_Superstore.xlsx")
head(ss, n=10)
summary(ss)
colnames(ss)

glimpse(ss)

ss <- clean_names(ss)
ss

sum(is.na(ss))
sapply(ss, class)


unique(ss$category)

ss %>% 
  group_by(category) %>% 
  summarise(total= n()) %>% 
  arrange(desc(total))

rlang::last_trace()


# Number of products in each category
ss %>% 
  group_by(category) %>% 
  dplyr::summarise(total=n()) %>% 
  arrange(desc(total))

n_distinct(ss$sub_category)
cols <- c("blue", "yellow", "purple")

ggplot(st, aes(x= sub_category, y = category, fill=category)) + 
  geom_col(position = position_dodge(),show.legend = FALSE)+ coord_flip()+ 
  theme_classic() + labs(title = "Category by Sub_Category",
                         subtitle ="Distribution of Subcategory")+
  scale_fill_manual(values = c("Furniture" = "#353436",
                               "Office Supplies" = "#1b98e0",
                               "Technology" = "red")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


ggplot(st, aes(x= sub_category, fill = category)) + 
  geom_bar() + theme_bw() + coord_flip() + 
  labs(title = "Count by Category and Sub_Category",
       x = "Sub_Category", y = "Frequency")+
  scale_fill_manual(values = c("Furniture" = "#353436",
                               "Office Supplies" = "#1b98e0",
                               "Technology" = "red")) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

  


ss %>% 
  group_by(state) %>% 
  dplyr::summarise(total_profit =sum(profit), total_discount = sum(discount)) %>% 
  pivot_longer(c("total_profit")) %>% 
  ggplot(aes(x=state, y = value, fill = name)) + 
  geom_col(position = position_dodge()) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Profit and Sales by state") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))



ss %>% 
  ggplot(aes(x=sub_category, fill= state)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Sub-Category Product Sales by City")+
  theme(plot.title = element_text(size=15, face="bold",hjust = 0.5))


ss <- ss %>% 
  mutate(costPrice = sales - profit)

ss <- ss %>% 
  mutate(profitpercent = (profit/costPrice *100))

summary(ss$sales)

summary(ss)


ss <- ss %>% 
  mutate(order_year = year(order_date))

ss$discount
ss %>% 
  group_by(category, order_year) %>% 
  dplyr::summarize(total_disc=sum(profit)) %>% 
  ggplot(aes(x= order_year, y= profit, color = category)) + 
  geom_point(size = 5, alpha=0.5) + geom_line(size=1) +
  theme_bw() +
  labs(title = "Profit Trend by Year for each Category",
       x= "Year", y = "Discount (percentage)")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

superstore %>% 
  ggplot(aes(x=sales, y=profit, color=discount))+
  geom_point(size=3) + geom_rug() + theme_bw() +
  labs(title="Sales,Profit and Discount Relationship")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5))

ss %>% 
  ggplot(aes(x=discount, fill=profit))+
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title="Profit and Discount Relationship")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5))


ss %>% 
  ggplot(aes(x=sub_category, fill= state)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Sub-Category Product Sales by City")+
  theme(plot.title = element_text(size=15, face="bold",hjust = 0.5))



st <- read_excel("C:/Users/tanya/Downloads/SampleSuperstore.xlsx")
st <- clean_names(st)
st

st %>% 
  group_by(product_name) %>% 
  dplyr::summarize(count_of_products=n()) %>% 
  arrange(desc(count_of_products)) %>% 
  slice_head(n=10) %>% 
  ggplot(superstore, 
         mapping = aes(x= product_name,
                       y= count_of_products))+
  geom_segment(aes(x = reorder(product_name, count_of_products), xend = product_name,
                   y = 0, yend = count_of_products), size =1,)+
  geom_point(size=10, color= "orange",
             shape = 20, stroke = 2) + coord_flip() + theme_bw() +
  labs(title = "Distribution of Top 10 Product",
       x = NULL) +
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5)) +
  geom_text(aes(label= count_of_products), 
            color="black", size=4, vjust=0.5, fontface='bold')


st %>% 
  group_by(region) %>% 
  dplyr::summarize(total_sales = round(sum(sales))) %>% 
  ggplot(aes(area = total_sales, fill = region, 
             label= paste0(region, "\n",prettyNum(total_sales, ",")))) +
  geom_treemap() + geom_treemap_text(color= "black", 
                                     place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Sales by Region") + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
