# LIBRARIES 
library(xlsx) # excel import
library(dplyr) # data data preprocessing and data wrangling
library(corrr) # simple correlation analysis
library(tsibble) # dealing with time-series tibbles
library(ggplot2) # general plotting
library(patchwork) # side-by-side plotting
library(lubridate) # advanced date operations
library(tidyverse) # advanced data wrangling

# LOADING THE DATA -----
sdat <- read.xlsx("attribution_data.xlsx", 
                  sheetIndex = 1, 
                  as.data.frame = T) %>% 
  mutate(ID = seq(1:nrow(.)))

# VARIABLE TRANSFORMATIONS ----
# introduce new channel variables
sdat <- sdat %>% 
  mutate(Channel = case_when(Groupname == "BUZZ AFFILIATE" ~ "Affiliate Marketing",
                             Groupname == "CJ" ~ "Affiliate Marketing",
                             Groupname == "CPM" ~ "Display Advertising",
                             Groupname == "SEARCH GOOGLE NON-BRAND" ~ "Search Engine",
                             Groupname == "SEARCH MSN NON-BRAND" ~ "Search Engine",
                             Groupname == "SEARCH GOOGLE BRAND" ~ "Search Engine",
                             Groupname == "SEARCH MSN BRAND" ~ "Search Engine",
                             Groupname == "SEARCH YAHOO BRAND" ~ "Search Engine",
                             Groupname == "SOCIAL" ~ "Social Media Sites",
                             Groupname == "Uncategorized" ~ "Uncategorized",
                             Groupname == "OTHER" | Groupname == "PRINT - MAGAZINES" |
                               Groupname == "TV" | Groupname == "DIRECT MAIL"~ "Other"))


# change to factors
sdat_fact <- sdat %>% 
  mutate(Newcustomer = as.factor(Newcustomer),
         Groupname = as.factor(Groupname),
         Brand = as.factor(Brand),
         Positionname = as.factor(Positionname),
         Channel = as.factor(Channel),
         TimeToConvert = difftime(Orderdatetime, Positiondatetime, unit = "hours"),
         TimeToConvert = as.numeric(TimeToConvert)) %>% 
  select(Orderid, Saleamount, Position, TimeToConvert, Channel, Groupname, Positionname, Newcustomer)
  


# PREPROCESSING -----
# TODO: needs to be mentioned in the beginning because later posiion and channel analyses will be wrong because no attribution assumptions


# real orders (everything is an order, just every touchpoint is recorded)
sdat %>% distinct(Orderid) %>% count() 

# there is never an order with only one touchpoint
sdat_fact %>% 
  group_by(Orderid) %>% 
  count() %>%
  ggplot(aes(n)) +
  stat_density() +
  labs(title = "Number of Positions per Actual Order",
       subtitle = "No order was finished with only one touchpoint",
       x = "Number of Touchpoints",
       y = "Density") +
  theme_bw()

# sales per order
sdat %>% group_by(Newcustomer, Orderid) %>% 
  summarise(sales = mean(Saleamount)) %>% 
  summarise(total_sales = sum(sales))

# distribution analysis, right tail revenue distribution seems normal
sdat %>% 
  pivot_longer(cols = c(Saleamount, Position),
               names_to = "variables",
               values_to = "values") %>% 
  ggplot(aes(values)) +
  stat_density() +
  facet_wrap(~variables, scales = "free", ncol = 1)

# relationship analysis
sdat_fact %>% 
  ggplot(aes(TimeToConvert, Saleamount)) +
  geom_point(alpha = 0.1) +
  geom_smooth(group = 1) +
  facet_wrap(~Channel + Newcustomer, ncol = 2) +
  theme_bw()

# relationship of position classes and actual positions
sdat_fact %>% 
  ggplot(aes(Position)) +
  stat_density() +
  facet_wrap(~Positionname)

# positions classification analysis
sdat_fact %>% 
  group_by(Newcustomer, Positionname) %>% 
  count() %>% 
  ggplot(aes(Positionname, n)) +
  geom_col() +
  facet_wrap(~Newcustomer)
  
sdat_fact %>% 
  ggplot(aes(Position, Positionname, fill = Positionname)) +
  geom_violin() +
  facet_wrap(~Newcustomer)

# TASK 1.1 ------
# old form including all channel
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

# same graph but with channel classifications (more Overview)

# TODO: size of labels,
# TODO: size of facets
# TODO. legend down
# TODO: delete x axis labels

sdat_fact %>% 
  group_by(Positionname, Channel) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  ggplot(aes(Positionname, n, fill = Positionname), colour = "white") +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.3) +
  facet_wrap(~Channel, nrow = 3) +
  coord_cartesian(ylim = c(0,3500)) +
  labs(x = "Position",
       y = "Channel Touchpoints",
       title = "Touchpoints per Channel and Position",
       subtitle = "Search Engines provide strong support for initial clicks, \nAffiliate Marketing serves a strong role in converting \nDisplay Advertisements are also strong in click assistance and conversion",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 8))

ggsave("01_1.1_Tocuhpoints per Channel and Position.png", width = 8, height = 6)




# TASK 1.2
# TODO. adjust data model
# TODO: different colors for close-up


c1 <- sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram() +
  facet_wrap(~Positionname, nrow = 2) +
  labs(x = "Time for Conversion [in hours]",
       y = "Count",
       title = "Conversion Time across Interval",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw()

# additionally split per hour
c2 <- sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  filter(TimeToConvert <= 24) %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram() +
  facet_wrap(~Positionname, nrow = 2) +
  labs(x = "Time for Conversion [in first 24 hours]",
       y = "Count",
       title = "Close-Up Look for Conversions within first 24 hours",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() 

c1 + c2

# amount of sales
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR"| Positionname == "CONVERTER") %>% 
  group_by(Positionname, Channel) %>% 
  summarise(sum_sales = sum(Saleamount)) %>% 
  arrange(desc(sum_sales)) %>% 
  ggplot(aes(Channel, sum_sales)) +
  geom_col() +
  facet_wrap(~Positionname, nrow = 2) +
  labs(x = "Channel",
       y = "Aggregated Sales",
       title = "Sales across different Channels and Positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 8))



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

# channel name adjustment for channels








# TASK 2.1
# FIRST CLICK ATTRIBUTION

attribution_results <- sdat_fact %>% 
  filter(Position == 0) %>% 
  group_by(Channel) %>% 
  summarise(first_click_sales = sum(Saleamount)) %>% 
  arrange(desc(first_click_sales))

# LAST CLICK ATTRIBUTION

attribution_results <- sdat_fact %>% 
  group_by(Orderid) %>% 
  arrange(desc(Orderid, Position)) %>%
  slice(1) %>% 
  group_by(Channel) %>% 
  summarise(last_click_sales = sum(Saleamount)) %>% 
  arrange(desc(last_click_sales)) %>% 
  left_join(attribution_results)

# EVEN ATTRIBUTION

attribution_results <- sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel) %>% 
  summarise(even_attribution_sales = sum(rev_share)) %>% 
  arrange(desc(even_attribution_sales)) %>% 
  left_join(attribution_results)

attribution_results %>% 
  pivot_longer(cols = c(even_attribution_sales, last_click_sales, first_click_sales),
               names_to = "attribution",
               values_to = "value") %>% 
  ggplot(aes(Channel, value)) +
  geom_col() +
  facet_wrap(~attribution, nrow = 3)


# TODO: fix y axis
# TODO: flip and make benchmarks easier
# TODO: add legend, title, etc.


# CASE FOR EVEN ATTRIBUTION MODEL
# check relationship between revenue and channel

sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel) %>% 
  ggplot(aes(Position, rev_share, colour = Channel)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~Channel) *+
  theme_bw()

sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel) %>% 
  ggplot(aes(rev_share, Position, fill = Channel)) +
  geom_violin() +
  # labs(title = "",
  #      subtitle = "Low") +
  facet_wrap(~Channel) +
  theme_bw() +
  
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))


# TASK 2.2

# TASK 2.3: DEVELOP YOUR OWN ATTRIBUTION MODEL

# regression ideas (skipped, does not work)
to_model <- sdat_fact %>% 
  select(Saleamount, Newcustomer, Position, Channel, TimeToConvert, Positionname)

m1 <- glm(Saleamount ~ ., data = to_model)

# position based attribution
sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         share = ifelse(Position == 1, 0.4,
                        ifelse(Position == n, 0.4, 0.2/(n-2))),
         rev_share = Saleamount * share) %>% 
  
  group_by(Channel) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  ggplot(aes(Channel, position_based_attribution)) +
  geom_col()

# TASK 2.4
# Same analysis as before split by type of customer
# FIRST CLICK ATTRIBUTION

attribution_results_nc <- sdat_fact %>% 
  filter(Position == 0) %>% 
  group_by(Channel, Newcustomer) %>% 
  summarise(first_click_sales = sum(Saleamount)) %>% 
  arrange(desc(first_click_sales))

# LAST CLICK ATTRIBUTION

attribution_results_nc <- sdat_fact %>% 
  group_by(Orderid) %>% 
  arrange(desc(Orderid, Position)) %>%
  slice(1) %>% 
  group_by(Channel, Newcustomer) %>% 
  summarise(last_click_sales = sum(Saleamount)) %>% 
  arrange(desc(last_click_sales)) %>% 
  left_join(attribution_results_nc)

# EVEN ATTRIBUTION

attribution_results_nc <- sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel, Newcustomer) %>% 
  summarise(even_attribution_sales = sum(rev_share)) %>% 
  arrange(desc(even_attribution_sales)) %>% 
  left_join(attribution_results_nc)

attribution_results_nc %>% 
  pivot_longer(cols = c(even_attribution_sales, last_click_sales, first_click_sales),
               names_to = "attribution",
               values_to = "value") %>% 
  ggplot(aes(Channel, value)) +
  geom_col() +
  facet_wrap(~attribution + Newcustomer, nrow = 3)

# position based attribution ----
sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         share = ifelse(Position == 1, 0.4,
                        ifelse(Position == n, 0.4, 0.2/(n-2))),
         rev_share = Saleamount * share) %>% 
  
  group_by(Channel, Newcustomer) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  ggplot(aes(Channel, position_based_attribution)) +
  geom_col() +
  facet_wrap(~Newcustomer)
# weighting is different


            