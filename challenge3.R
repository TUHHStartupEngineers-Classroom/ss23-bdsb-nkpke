# Challenge 3 ----

# 1.0 LIBRARIES ----

# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# 2.0 DATA ----

# Import assignee table
col_types <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
            file       = "./patents/assignee.tsv", 
            delim      = "\t", 
            col_types  = col_types,
            na         = c("", "NA", "NULL")
        )

# import patent table
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

# Import patent assignee table
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_asignee_tbl <- vroom(
            file       = "./patents/patent_assignee.tsv", 
            delim      = "\t", 
            col_types  = col_types,
            na         = c("", "NA", "NULL")
        )

# Convert to data.table
class(assignee_tbl)
setDT(assignee_tbl)
class(assignee_tbl)

class(patent_tbl)
setDT(patent_tbl)
class(patent_tbl)

class(patent_asignee_tbl)
setDT(patent_asignee_tbl)
class(patent_asignee_tbl)

# 3.0 ANALYSIS ----
# rename id in assignee table to assignee_id
assignee_tbl <- assignee_tbl %>% rename(assignee_id = id)

# Write organization names to the respective id
combi_data <- merge(x = patent_asignee_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

# Companies with the most patents:
patent_by_company_tbl <-combi_data[!is.na(assignee_id), .(sum_patents = .N), by = assignee_id][
  order(sum_patents, decreasing = TRUE)]

# re add the company names 
patent_by_company_tbl <- merge(x = patent_by_company_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)

# rearrange
patent_by_company_tbl <- patent_by_company_tbl %>% select(organization, sum_patents, assignee_id)

# order
patent_by_company_tbl <- patent_by_company_tbl[order(sum_patents, decreasing = TRUE)]

# top 10 
head(patent_by_company_tbl, 10)
