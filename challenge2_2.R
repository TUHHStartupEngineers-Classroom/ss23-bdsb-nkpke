 # Challenge 2.2 ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(RSQLite)
library(httr)
library(xml2)

# URL of shop
base_url <- "https://www.rosebikes.com"

# Collecting info about bikes of MTB category
mtb_url = glue("{base_url}/bikes/mtb")

# Open mtb url
# xopen(mtb_url)

# create table and extract href for model from html
mtb_tbl <-  read_html(mtb_url) %>% 
  html_nodes(css = ".catalog-category-bikes__picture-wrapper") %>% 
  map(xml_attrs) %>% 
  map("href") %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename("modelurl" = value) %>%
  # Add the domain, because we will get only the subdirectories
  mutate(modelurl = glue("{base_url}{modelurl}"))  %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(modelurl)

# extract href for bikes of html for every url in mtb_tbl
bike_tbl <- mtb_tbl$modelurl %>% 
  map(read_html) %>% 
  map(html_nodes, css = ".catalog-category-model__picture-link") %>% 
  map(xml_attrs) 

# flatten nested lists
bike_tbl <- unlist(bike_tbl, recursive=FALSE)

# extract href from all bikes
bike_tbl <- bike_tbl %>% 
  map("href") %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename("bikeurl" = value) %>%
  mutate(bikeurl = glue("{base_url}{bikeurl}"))  %>%
 
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(bikeurl)


# removing entries that give errors
bike_tbl <- bike_tbl[-8,]
bike_tbl <- bike_tbl[-52,]

# save first entries of bike_tbl to variable for testing
# bike_tbl <- bike_tbl[1:4,]  %>%   as_tibble()

price_tbl <- bike_tbl$bikeurl %>% 
  map(read_html) %>%
  map(html_nodes, css = ".detail-price__wrapper > span") %>% 
  map(xml_attrs)

# flatten nested lists
price_tbl <- unlist(price_tbl, recursive=FALSE)

# extract price from all entries
price_tbl <- price_tbl %>% 
  map("data-test") %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename("price" = value)

# remove dollar signs from price 
price_tbl$price <- price_tbl$price %>% 
  str_remove_all("€") %>% 
  str_remove_all("\\.") %>% 
  as.numeric()

name_tbl <- bike_tbl$bikeurl %>% 
  map(read_html) %>%
  map(html_nodes, ".basic-headline__title") %>% 
  map(xml_find_all, ".//text()")

# for loop through name_tbl to get only the text 
for (i in 1:length(name_tbl)) {
  name_tbl[[i]] <- as_list(name_tbl[[i]])[[1]]
}

name_tbl <- name_tbl %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename("name" = value)

# combine rows from name_tbl and price_tbl
combi_tbl <- bind_cols(name_tbl, price_tbl)

# preview first 10 rows
combi_tbl %>% head(10)
