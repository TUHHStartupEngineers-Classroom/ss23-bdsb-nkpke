# Challenge 3 ----

# 1.0 LIBRARIES ----

# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)


col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
            file       = "./patents/patent.tsv", 
            delim      = "\t", 
            col_types  = col_types,
            na         = c("", "NA", "NULL")
        )
