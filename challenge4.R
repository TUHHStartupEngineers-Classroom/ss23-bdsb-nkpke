# Challenge 4 

# 1.0 Load Libraries ----
library(tidyverse)
library(ggthemes)

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct()

# for each day, add total cases of all countries
covid_data_tbl <- covid_data_tbl %>%
  filter(date < "2023-05-15") %>%
  group_by(date) %>%
  mutate(global_total_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup()

# line plot of global total cases over time
covid_data_tbl %>%
  ggplot(aes(date, global_total_cases)) +
  geom_line() +
  labs(
    title = "Global Total Cases over Time",
    subtitle = "Source: Our World in Data",
    x = "Date",
    y = "Total Cases"
  ) +
  theme_grey()

# Create column with mortility rate (total deaths / population)
covid_data_tbl <- covid_data_tbl %>%
  mutate(mortality_rate = total_deaths / population)

world <- map_data("world")

# Visualise mortatility rate on a map
covid_data_tbl %>%
  filter(date == "2023-05-01") %>%
  ggplot(aes(mortality_rate)) +
  geom_map(aes(fill = mortality_rate, map_id = location), map = world) +
  expand_limits(x = world$long, y = world$lat) +
  coord_map("moll") +
  scale_fill_gradient2(low = "green", mid = "grey", high = "red", midpoint = 0.003) +
  labs(
    title = "Mortality Rate by Country",
    subtitle = "Source: Our World in Data",
    x = "",
    y = ""
  ) +
  theme_map()
