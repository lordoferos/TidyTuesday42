library(readr)
library(tidyverse)
library(ggpubr)

big_epa_cars <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

#Check the 10 most common make of vehicles
car_num = 
  big_epa_cars %>%
  select(make) %>%
  group_by(make)%>%
  summarise(n=n())%>% 
  arrange(desc(n))%>% 
  filter(n>1078)#number less than 10th

#Dataset with the 10 most common vehicle makes
car_com =
  big_epa_cars%>%
  filter(make %in% c("Chevrolet","Ford", "Dodge","GMC","Toyota","BMW",
                     "Mercedes-Benz","Nissan","Volkswagen","Porsche"))

#Create barplots with fuel cost
ggboxplot(car_com, x = "make", y = "fuelCost08",
               color = "make", palette = "Vividis",
               outlier.shape = NA) + 
  geom_hline(yintercept = median(car_com$fuelCost08))+
    labs(x = "Make of car", y = "Annual cost for Fuel Type 1 ($)",
       title = "Fuel consumption of the 10 most popular cars",
       caption = "Data Source: https://www.fueleconomy.gov") +
  theme_bw()+
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

