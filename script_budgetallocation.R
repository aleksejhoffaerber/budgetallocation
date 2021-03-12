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

# TODO: Uncategorized as "Other" ??
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
         Position = as.factor(Position),
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

ggsave("01_1.1_Touchpoints per Channel and Position.png", width = 8, height = 6)




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

ggsave("01_1.2_Conversion Time Differences.png", width = 8, height = 6, dpi = 600)


# amount of sales
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR"| Positionname == "CONVERTER") %>% 
  group_by(Positionname, Channel) %>% 
  summarise(sum_sales = sum(Saleamount)) %>% 
  arrange(desc(sum_sales)) %>% 
  ggplot(aes(Channel, sum_sales, fill = Channel)) +
  geom_col() +
  facet_wrap(~Positionname, nrow = 2) +
  labs(x = "Channel",
       y = "Aggregated Sales",
       title = "Sales across different Channels and Positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90),
  #       strip.text.x = element_text(size = 8)) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.2_Sales across different Channels and Positions.png", width = 8, height = 6, dpi = 600)



# TASK 1.3
# conversion times

mean_conversion_time <- sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_conversion_time = mean(TimeToConvert))

sdat_fact %>% 
  group_by(Newcustomer, Channel) %>% 
  summarise(mean_conversion_time = mean(TimeToConvert))

# conversion time comparison
sdat_fact %>% 
  ggplot(aes(Position, TimeToConvert, fill = Newcustomer)) +
  geom_boxplot(outlier.alpha = .1) +
  
  geom_hline(yintercept = mean_conversion_time[[2,"mean_conversion_time"]], 
             linetype = "dashed", colour = "#00BFC4", size = 1) +
  annotate(geom = "label", x = 9.5, y = mean_conversion_time[[2,"mean_conversion_time"]] + 200, 
           label = round(mean_conversion_time[[2,"mean_conversion_time"]],1), colour = "#00BFC4") +
  
  geom_hline(yintercept = mean_conversion_time[[1,"mean_conversion_time"]], 
             linetype = "dashed", colour = "#F8766D", size = 1) +
  annotate(geom = "label", x = 9.5, y = mean_conversion_time[[1,"mean_conversion_time"]] + 200, 
           label = round(mean_conversion_time[[1,"mean_conversion_time"]],1), colour = "#F8766D") +
  
  facet_wrap(~ Newcustomer) +
  labs(x = "Position",
       y = "Time to Convert [in hours]",
       title = "Difference in Conversion Times by Customer Type",
       subtitle = "New Customers show significantly faster conversion times across all positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.3_Difference in Conversion Times by Customer Type.png", width = 8, height = 6, dpi = 600)



# APPENDIX additional difference by channel
sdat_fact %>% 
  ggplot(aes(Position, TimeToConvert, fill = Newcustomer)) +
  geom_boxplot(outlier.alpha = .1) +
  
  facet_wrap(~ Channel + Newcustomer) +
  labs(x = "Position",
       y = "Time to Convert [in hours]",
       title = "Difference in Conversion Times by Customer Type and Channel",
       subtitle = "New Customers show significantly faster conversion times across all positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))


# spending behavior

mean_spending <- sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_spending = mean(Saleamount))

sdat_fact %>% 
  group_by(Newcustomer, Channel) %>% 
  summarise(mean_conversion_time = mean(Saleamount))


# spending  comparison
sdat_fact %>% 
  ggplot(aes(Position, Saleamount, fill = Newcustomer)) +
  geom_boxplot(outlier.alpha = .1) +
  
  geom_hline(yintercept = mean_spending[[2,"mean_spending"]],
             linetype = "dashed", colour = "#00BFC4", size = 1) +
  annotate(geom = "label", x = 9.5, y = mean_spending[[2,"mean_spending"]] + 100,
           label = round(mean_spending[[2,"mean_spending"]],1), colour = "#00BFC4") +

  geom_hline(yintercept = mean_spending[[1,"mean_spending"]],
             linetype = "dashed", colour = "#F8766D", size = 1) +
  annotate(geom = "label", x = 9.5, y = mean_spending[[1,"mean_spending"]] - 100,
           label = round(mean_spending[[1,"mean_spending"]],1), colour = "#F8766D") +
  
  facet_wrap(~ Newcustomer) +
  labs(x = "Position",
       y = "Sales",
       title = "Difference in Sales by Customer Type and Channel",
       subtitle = "New Customers show significantly higher sales across all positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.3_Difference in Sales by Customer Type and Channel.png", width = 8, height = 6, dpi = 600)



# different channels as ORIGINATOR
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR") %>% 
  group_by(Newcustomer, Channel) %>% 
  count() %>% 
  ggplot(aes(Channel, n, fill = Channel)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.5) +
  facet_wrap(~Newcustomer, nrow = 2) +
  coord_cartesian(ylim = c(0,650)) +
  labs(x = "Channel",
       y = "Number of Touchpoints",
       title = "Deep-Dive Originators | Touchpoints per Channel, split by New and Old Customers",
       subtitle = "More New Customers originated from Display Advertising, Search Engines and Affilitate Marketing",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.3_New Customers Originators.png", width = 8, height = 6, dpi = 600)


# different channels as CONVERTER

sdat_fact %>% 
  filter(Positionname == "CONVERTER") %>% 
  group_by(Newcustomer, Channel) %>% 
  count() %>% 
  ggplot(aes(Channel, n, fill = Channel)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.5) +
  facet_wrap(~Newcustomer, nrow = 2) +
  coord_cartesian(ylim = c(0,800)) +
  labs(x = "Channel",
       y = "Number of Touchpoints",
       title = "Deep-Dive Converter | Touchpoints per Channel, split by New and Old Customers",
       subtitle = "Considerable more New Customers converted via Affiliate Marketing and Display Advertising",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.3_New Customer Converters.png", width = 8, height = 6, dpi = 600)



## PART II: -----

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


# graph
l1 <- c("Even Attribution", "First Click Attribution", "Last Click Attribution")
names(l1) <- c("even_attribution_sales", "first_click_sales", "last_click_sales")

attribution_results %>% 
  pivot_longer(cols = c(even_attribution_sales, last_click_sales, first_click_sales),
               names_to = "attribution",
               values_to = "value") %>%
  mutate(value = round(value),1) %>% 
  ggplot(aes(Channel, value, fill = Channel)) +
  geom_col() +
  geom_label(aes(label = value), vjust = -.5) +
  facet_wrap(~attribution, nrow = 3,
             labeller = labeller(attribution = l1)) +
  coord_cartesian(ylim = c(0,320000)) +
  labs(x = "Channel",
       y = "Sales",
       title = "Comparison between Attribution Strategies",
       subtitle = "Depending on the strategy, different channels in the customer journey are prioritized",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_2.1_Comparison between Attribution Strategies.png", width = 8, height = 6, dpi = 600)

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
  facet_wrap(~Channel) +
  theme_bw()


# FIXME: For what? Title etc.
sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel) %>% 
  ggplot(aes(Position, rev_share, fill = Channel)) +
  geom_violin() +
  labs(title = "",
       subtitle = "Low") +
  facet_wrap(~Channel) +
  theme_bw() +
  
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))


# TASK 2.2

s1 <- sdat_fact %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(rev_share = Saleamount/n) %>% 
  group_by(Channel, Positionname) %>% 
  summarise(even_attribution_sales = sum(rev_share)) %>% 
  arrange(desc(even_attribution_sales)) %>% 
  mutate(even_attribution_sales = round(even_attribution_sales),1) %>% 
  ggplot(aes(Channel, even_attribution_sales, fill = Channel)) +
  geom_col() +
  geom_label(aes(label = even_attribution_sales), vjust = -.5) +
  facet_wrap(~Positionname, nrow = 4) +
  coord_cartesian(ylim = c(0, 160000)) +
  labs(x = "Channel",
       y = "Sales",
       title = "Even Attribution Model | Detailed View",
       subtitle = "Depending on the strategy, different channels in the customer journey are prioritized",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_2.2_Even Attribution Model - Detailed View.png", width = 8, height = 6, dpi = 600)




# TASK 2.3: DEVELOP YOUR OWN ATTRIBUTION MODEL

# position based attribution
s2 <- sdat %>% 
  # filter(!Channel %in% c("Uncategorized", NA, "Other")) %>% # does not work, sometimes negative values because of the share formula
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         # heuristic based on:
         share = case_when(n == 2 ~ 0.5, # shares equal 50% if only 2 positions (min)
                           Position == 1 ~ 0.4, # otherwise first position = 40%
                           Position == n ~ 0.4, # and last position = 40%
                           TRUE ~ 0.2/(n-2)), # rest shares 20%
         rev_share = Saleamount * share)  %>% 
  
  group_by(Channel, Positionname) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  mutate(position_based_attribution = round(position_based_attribution,1)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  ggplot(aes(Channel, position_based_attribution, fill = Channel)) +
  geom_col() +
  geom_label(aes(label = position_based_attribution), vjust = -.5) +
  coord_cartesian(ylim = c(0, 160000)) +
  # additionally one could include Newcustomer
  facet_wrap(~Positionname, nrow = 4) +
  
  labs(x = "Channel",
       y = "Sales",
       title = "Position-Based Attribution Strategy | Detailed View",
       subtitle = "Detailed view shows that PB shows a budget allocation in favor of both, converters and originators",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

s1 + s2

ggsave("01_2.3_Position-Based Attribution Strategy.png", width = 8, height = 6, dpi = 600)


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
sdat %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         # heuristic based on:
         share = case_when(n == 2 ~ 0.5, # shares equal 50% if only 2 positions (min)
                           Position == 1 ~ 0.4, # otherwise first position = 40%
                           Position == n ~ 0.4, # and last position = 40%
                           TRUE ~ 0.2/(n-2)), # rest shares 20%
         rev_share = Saleamount * share)  %>% 
  
  group_by(Channel, Positionname) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  ggplot(aes(Channel, position_based_attribution, fill = Channel)) +
  geom_col() +
  # additionally one could include Newcustomer
  facet_wrap(~Positionname, nrow = 4) +
  
  labs(x = "Channel",
       y = "Sales",
       title = "Comparison between Attribution Strategies",
       subtitle = "Depending on the strategy, different channels in the customer journey are prioritized",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_2.4_Comparison between Attribution Strategies.png", width = 8, height = 6, dpi = 600)

  
  
# weighting is different


            