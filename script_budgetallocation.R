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
  facet_wrap(~Positionname) +
  labs(x = "Position",
       y = "Density",
       title = "Distribution of Positions across Position Classifications",
       subtitle = "Different position classification play a role across all positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_00_Distribution of Positions across Position Classifications.png", width = 8, height = 3)



# TASK 1.1 ------
# old form including all channel
sdat_fact %>%
  group_by(Positionname, Groupname) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(Groupname, n, fill = Groupname), colour = "white") +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.3, label.size = 0.05, label.r = unit(0.05, "lines")) +
  facet_wrap(~Positionname, nrow = 3) +
  coord_cartesian(ylim = c(0,3500)) +
  labs(x = "Position",
       y = "Channel Touchpoints",
       title = "Touchpoints per Channel and Position",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",
        text = element_text(size = 8)) +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.1_Touchpoints per Touchpoint Channel and Position.png", width = 12, height = 6)

# TASK 1.2

c1 <- sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram(fill = "#F8766D") +
  facet_wrap(~Positionname, nrow = 1) +
  labs(x = "Time for Conversion [in hours]",
       y = "Count",
       title = "Conversion Time across Interval") +
  theme_bw()

# additionally split per hour
c2 <- sdat_fact %>% 
  filter(Positionname == "ORIGINATOR" | Positionname == "CONVERTER") %>% 
  filter(TimeToConvert <= 24) %>% 
  ggplot(aes(TimeToConvert)) +
  geom_histogram(fill = "#00BFC4") +
  facet_wrap(~Positionname, nrow = 1) +
  labs(x = "Time for Conversion [in first 24 hours]",
       y = "Count",
       title = "Close-Up Look for Conversions within first 24 hours",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() 

c1 / c2

ggsave("01_1.2_Conversion Time Differences.png", width = 12, height = 8, dpi = 600)


# amount of sales
sdat_fact %>% 
  filter(Positionname == "ORIGINATOR"| Positionname == "CONVERTER") %>% 
  group_by(Positionname, Channel) %>% 
  summarise(sum_sales = sum(Saleamount)) %>% 
  arrange(desc(sum_sales)) %>% 
  mutate(sum_sales = round(sum_sales),0) %>% 
  ggplot(aes(Channel, sum_sales, fill = Channel)) +
  geom_col() +
  geom_label(aes(label = sum_sales), vjust = -.5) +
  facet_wrap(~Positionname, nrow = 2) +
  coord_cartesian(ylim = c(0,380000)) +
  labs(x = "Channel",
       y = "Aggregated Sales",
       title = "Sales across different Channels and Positions",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.2_Sales across different Channels and Positions.png", width = 12, height = 5, dpi = 600)



# TASK 1.3
# conversion times

mean_conversion_time <- sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_conversion_time = mean(TimeToConvert))

sdat_fact %>% 
  group_by(Newcustomer, Channel) %>% 
  summarise(mean_conversion_time = mean(TimeToConvert)) 
  

# spending behavior

mean_spending <- sdat_fact %>% 
  group_by(Newcustomer) %>% 
  summarise(mean_spending = mean(Saleamount))

sdat_fact %>% 
  group_by(Newcustomer, Channel) %>% 
  summarise(mean_sales = mean(Saleamount))




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
  filter(Positionname == "CONVERTER" | Positionname == "ORIGINATOR") %>% 
  group_by(Newcustomer, Channel, Positionname) %>% 
  count() %>% 
  ggplot(aes(Channel, n, fill = Channel)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n), vjust = -0.5) +
  facet_wrap(~Newcustomer + Positionname, nrow = 2) +
  coord_cartesian(ylim = c(0,800)) +
  labs(x = "Channel",
       y = "Number of Touchpoints",
       title = "Touchpoints per Channel, split by New and Old Customers",
       subtitle = "Converters: Considerable more New Customers converted via Affiliate Marketing and Display Advertising \nOriginators: More New Customers originated from Display Advertising, Search Engines and Affilitate Marketing",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_1.3_New Customer Converters and Originators.png", width = 12, height = 6, dpi = 600)



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

ggsave("01_2.1_Comparison between Attribution Strategies.png", width = 12, height = 6, dpi = 600)

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


# TASK 2.3: DEVELOP YOUR OWN ATTRIBUTION MODEL

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
  theme(legend.position = "none",
        axis.text.x = element_blank())
  # theme(axis.text.x = element_blank(),
  #       legend.position = "bottom", legend.box = "horizontal") +
  # scale_color_discrete(NULL) + 
  # guides(colour = guide_legend(nrow = 1))

ggsave("01_2.2_Even Attribution Model - Detailed View.png", width = 8, height = 6, dpi = 600)


# position based attribution
s2 <- sdat %>% 
  # filter(!Channel %in% c("Uncategorized", NA, "Other")) %>% # does not work, sometimes negative values because of the share formula
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         # heuristic based on:
         share = case_when(n == 2 ~ 0.5, # shares equal 50% if only 2 positions (min)
                           Position == 1 ~ 0.35, # otherwise first position = 35%
                           Position == n ~ 0.35, # and last position = 35%
                           TRUE ~ 0.3/(n-2)), # rest shares 30%
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
  theme_bw() 

s1 + s2 +   
  theme(axis.text.x = element_blank(),
        legend.position = "right", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_2.3_Attribution Strategy Comparison.png", width = 12, height = 6, dpi = 300)

# TODO: make a full simulation of all k-values between 45 and 5

k <- 0.35

sdat %>% 
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         # heuristic based on:
         share = case_when(n == 2 ~ 0.5, # shares equal 50% if only 2 positions (min)
                           Position == 1 ~ k, # otherwise first position = 35%
                           Position == n ~ k, # and last position = 35%
                           TRUE ~ (1-2*k)/(n-2)), # rest shares 30%
         rev_share = Saleamount * share)  %>% 
  
  group_by(Channel) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  mutate(position_based_attribution = round(position_based_attribution,1)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  ggplot(aes(Channel, position_based_attribution, fill = Channel)) +
  geom_col() +
  geom_label(aes(label = position_based_attribution), vjust = -.5) +
  coord_cartesian(ylim = c(0, 300000)) +
  # additionally one could include Newcustomer
  # facet_wrap(~Positionname, nrow = 4) +
  
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




# TASK 2.4
# Same analysis as before split by type of customer

sdat_fact %>% 
  group_by(Newcustomer, Positionname, Channel) %>% 
  count() %>% 
  ggplot(aes(Channel, n, fill = Channel)) +
  geom_col() +
  facet_wrap(~Positionname + Newcustomer, nrow = 2) +
  labs(x = "Position Classification",
       y = "Number of Touchpoints",
       title = "Difference in Touchpoint Attribution by Type of Customer",
       subtitle = "Dataset includes more new customers than existing customers, which illustrated by the different channels",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

sdat_fact %>% 
  group_by(Newcustomer, Position, Channel) %>% 
  count() %>% 
  ggplot(aes(Position, n, fill = Channel)) +
  geom_col() +
  facet_wrap(~Channel + Newcustomer, nrow = 2) +
  labs(x = "Position",
       y = "Occurences",
       title = "Occurence of Positions between Type of Customers and Channel",
       subtitle = "New customers have a similar position distribution, channel prioritization can be kept",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))

ggsave("01_2.4_Position Comparison between Channels and Type of Customers.png", width = 12, height = 6, dpi = 300)


# TASK 2.5. BUDGET ALLOCATION

budgetallocation <- sdat %>% 
  # filter(!Channel %in% c("Uncategorized", NA, "Other")) %>% # does not work, sometimes negative values because of the share formula
  group_by(Orderid) %>% 
  add_tally() %>% 
  mutate(Position = Position + 1,
         # heuristic based on:
         share = case_when(n == 2 ~ 0.5, # shares equal 50% if only 2 positions (min)
                           Position == 1 ~ k, # otherwise first position = 35%
                           Position == n ~ k, # and last position = 35%
                           TRUE ~ (1-2*k)/(n-2)), # rest shares 30%
         rev_share = Saleamount * share)  %>% 
  
  group_by(Channel) %>% 
  summarise(position_based_attribution = sum(rev_share)) %>% 
  mutate(position_based_attribution = round(position_based_attribution,1)) %>% 
  arrange(desc(position_based_attribution)) %>% 
  
  mutate(share = round((position_based_attribution / sum(position_based_attribution))*100,2))

budgetallocation_adj <- budgetallocation %>% 
  filter(Channel %in% c("Display Advertising", "Affiliate Marketing", "Search Engine")) %>% 
  mutate(share_adj = share + (share / (45.04+32.23+19.99)) * (1.69+0.98+0.07))


# APPENDIX:

# Appendix 1:

sdat_fact %>% 
  group_by(Positionname, Channel) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  group_by(Positionname) %>% 
  mutate(share = round(((n / sum(n)) * 100),2)) %>% 
  
  ggplot(aes(Channel, share, fill = Channel), colour = "white") +
  geom_bar(stat = "identity") +
  geom_label(aes(label = share), vjust = -0.3) +
  facet_wrap(~Positionname, nrow = 3) +
  coord_cartesian(ylim = c(0,100)) +
  labs(x = "Position",
       y = "Channel Touchpoints [in percent]",
       title = "Touchpoints per Channel and Position [in percent]",
       subtitle = "Search Engines provide strong support for initial clicks, \nAffiliate Marketing serves a strong role in converting and rosting (retargeting) \nDisplay Advertisements are strong across all categories",
       caption = "Source: W.M. Winters, May to June 2012") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom", legend.box = "horizontal") +
  scale_color_discrete(NULL) + 
  guides(colour = guide_legend(nrow = 1))


ggsave("01_A1_Touchpoints per Channel and Position in percent.png", width = 12, height = 6)


# Appendix 2: 

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

ggsave("01_A2_Difference in Conversion Times by Customer Type.png", width = 12, height = 4, dpi = 600)


# Appendix 3:

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

ggsave("01_A3_Difference in Sales by Customer Type and Channel.png", width = 12, height = 4, dpi = 600)


# Appendix 4: 
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

ggsave("01_A4_Difference in Conversion Times by Customer Type and Channel.png", width = 12, height = 10, dpi = 600)
