# libraries 
library(xlsx) # excel import
library(dplyr) # data data preprocessing and data wrangling
library(corrr) # simple correlation analysis
library(tsibble) # dealing with time-series tibbles
library(ggplot2) # general plotting
library(patchwork) # side-by-side plotting
library(lubridate) # advanced date operations
library(tidyverse) # advanced data wrangling

# load the data 
sdat <- read.xlsx("attribution_data.xlsx", 
                  sheetIndex = 1, 
                  as.data.frame = T) %>% 
  mutate(ID = seq(1:nrow(.)))

# analyse and preprocess
# TODO: data preprocessing
# TODO: data analysis

# do not change to factor, because it changes the data
sdat_fact <- sdat %>% 
  mutate(Newcustomer = as.factor(Newcustomer),
         Groupname = as.factor(Groupname),
         Brand = as.factor(Brand),
         Positionname = as.factor(Positionname)) 
  

# TASK 1.1
sdat_fact %>% 
  group_by(Positionname, Groupname) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  ggplot(aes(Positionname, n, fill = Positionname), colour = "white") +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.3) +
  facet_wrap(~Groupname, nrow = 3) +
  coord_cartesian(ylim = c(0,3500)) +
  labs(x = "Position",
       y = "Channel Touchpoints",
       title = "Touchpoints per Channel and Position",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 5))

ggsave("01_1.1_Tocuhpoints per Channel and Position.png", width = 8, height = 6)

# TODO: size of labels,
# TODO: size of facets
# TODO. legend down


# TASK 1.2
# add difftime into df

sdat_fact <- sdat_fact %>% 
  mutate(TimeToConvert = difftime(Orderdatetime, Positiondatetime, unit = "days"))

# difftime

# TODO. adjust data model
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  mutate(TimeToConvert = difftime(Orderdatetime, Positiondatetime, unit = "days")) %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram() +
  facet_wrap(~Positionname, nrow = 2)

# additionally split per hour
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  mutate(TimeToConvert = difftime(Orderdatetime, Positiondatetime, unit = "hours")) %>% 
  filter(TimeToConvert <= 24) %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram() +
  facet_wrap(~Positionname, nrow = 2)

# amount of sales
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR"| Positionname == "CONVERTER") %>% 
  mutate(TimeToConvert = difftime(Orderdatetime, Positiondatetime, unit = "days")) %>% 
  group_by(Positionname, Groupname) %>% 
  summarise(sum_sales = sum(Saleamount)) %>% 
  arrange(desc(sum_sales)) %>% 
  ggplot(aes(Groupname, sum_sales)) +
  geom_col() +
  facet_wrap(~Positionname, nrow = 2) +
  labs(x = "Channel",
       y = "Aggregated Sales",
       title = "Sales across different Channels and Positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 5))



# TASK 1.3
# conversion times

sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_conversion_time = mean(TimeToConvert))

# spending behavior

sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_spend = mean(Saleamount))

# different channels as ORIGINATOR
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR") %>% 
  group_by(Newcustomer, Groupname) %>% 
  count() %>% 
  ggplot(aes(Groupname, n)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.5) +
  facet_wrap(~Newcustomer, nrow = 2) +
  coord_cartesian(ylim = c(0,600)) +
  labs(x = "Channel",
       y = "Number of Touchpoints",
       title = "Touchpoints per Channel, split by New and Old Customers",
       subtitle = "Considerable more New Customers converted via CPM and Google Search - despite the difference in group affiliation",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 8))

# TODO: compute actual percentages
# TODO: change caption of facet wrap

# different channels as CONVERTER

sdat_fact %>% 
  filter(Positionname == "CONVERTER") %>% 
  group_by(Newcustomer, Groupname) %>% 
  count() %>% 
  ggplot(aes(Groupname, n)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.5) +
  facet_wrap(~Newcustomer, nrow = 2) +
  coord_cartesian(ylim = c(0,600)) +
  labs(x = "Channel",
       y = "Number of Touchpoints",
       title = "Touchpoints per Channel, split by New and Old Customers",
       subtitle = "Considerable more New Customers converted via CPM and Google Search - despite the difference in group affiliation",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 8))


## PART II:



