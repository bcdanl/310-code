# install.packages(c("tidyverse", "socviz", "ggthemes", "hrbrthemes", "skimr"))

library(tidyverse)
library(socviz)
library(ggthemes)

climate_opinion_long <- read_csv(
  'https://bcdanl.github.io/data/climate_opinion_2021.csv')


# CW 8
# Q1
# Filter climate_opinion_long, 
# so that climate_opinion_long has only estimated percentage of people 
# who think that global warming is caused mostly by human activities.


q1 <- climate_opinion_long |> 
  filter(belief == 'human')


#Q2

county_map <- socviz::county_map |> 
  mutate(id = as.numeric(id))

q2 <- county_map |> 
  left_join(q1)


# Q3


party_colors <- c("#2E74C0", "#CB454A")  # Hex color codes for Dem Blue and Rep Red
brk = as.vector(
  round(
  c(min(q2$perc, na.rm = T),
        quantile(q2$perc, .25, na.rm = T),
        median(q2$perc, na.rm = T),
        quantile(q2$perc, .75, na.rm = T),
        max(q2$perc, na.rm = T)
), 2)
)

lab <- as.vector(c(paste0(min(q2$perc, na.rm = T), '\n (Min)'),
  paste0(quantile(q2$perc, .25, na.rm = T), '\n (25th)'),
  paste0(median(q2$perc, na.rm = T), '\n (50th)'),
  paste0(quantile(q2$perc, .75, na.rm = T), '\n (75th)'),
  paste0(quantile(q2$perc, 1, na.rm = T), '\n (Max)')
))

q2 |> 
  ggplot(aes(x = long, y = lat, group = group,
             fill = perc)) +
  geom_polygon() +
  scale_fill_gradient2(low = '#2E74C0',
                       mid = 'white',
                       high = '#CB454A',
                       midpoint = median(q2$perc, na.rm = T),
                       breaks = brk,
                       label = lab) +
  guides(fill = guide_colorbar(barwidth = 20)) +
  labs(caption = 'example\n web address') +
  theme_map() +
  theme(legend.position = c(.5, .1),
        legend.justification = 'center',
        plot.caption = element_text(margin = margin(-100,0,0,0))
        ) 
  


# Q3
library(lubridate)
unemp_house_prices <- read_csv(
  'https://bcdanl.github.io/data/unemp_house_prices.csv')

q3 <- unemp_house_prices |> 
  filter(year(date) >= 2008)


# install.packages("geofacet")
library(geofacet)


adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)

q3 |> 
  ggplot(aes(x = date, y = unemploy_perc)) +
  geom_area(fill = '#2E74C0', color = 'black', alpha = .5) +
  facet_geo(~state, labeller = adjust_labels) +
  scale_x_date(
    breaks = c(ymd("2009-01-01"), ymd("2011-01-01"), ymd("2013-01-01"), 
               ymd("2015-01-01"), ymd("2017-01-01")),
    labels = c("'09", "'11", "'13", "'15", "'17"), 
    )

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




