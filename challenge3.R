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

# Import uspc table 
col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
            file       = "./patents/uspc.tsv", 
            delim      = "\t", 
            col_types  = col_types,
            na         = c("", "NA", "NULL")
        )

# Challenge 3.1 
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

  class(uspc_tbl)
  setDT(uspc_tbl)
  class(uspc_tbl)
  
  # 3.0 ANALYSIS ----
  # rename id in assignee table to assignee_id
  assignee_tbl <- assignee_tbl %>% rename(assignee_id = id)
 
  # rename id in patent table to patent_id
  patent_tbl <- patent_tbl %>% rename(patent_id = id)
  
  # Write organization names to the respective id
  combi_data <- merge(x = patent_asignee_tbl, y = assignee_tbl, 
                         by    = "assignee_id", 
                         all.x = TRUE, 
                         all.y = FALSE)
  
  # Companies with the most patents:
  patent_by_company_tbl <-combi_data[!is.na(assignee_id), .(sum_patents = .N), by = assignee_id]
  
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

# Challenge 3.2
  # Add date to patent_asignee_tbl 
  combi_data1 <- merge(x = combi_data, y = patent_tbl, 
                         by    = "patent_id", 
                         all.x = TRUE, 
                         all.y = FALSE)

  # Companies with the most patents in April 
  patent_by_company_april_tbl <-combi_data1[month(date) == 4 ][!is.na(assignee_id), .(sum_patents = .N), by = assignee_id]

  # re add the company names 
  patent_by_company_april_tbl <- merge(x = patent_by_company_april_tbl, y = assignee_tbl, 
                         by    = "assignee_id", 
                         all.x = TRUE, 
                         all.y = FALSE)
  
  # rearrange
  patent_by_company_april_tbl <- patent_by_company_april_tbl %>% select(organization, sum_patents, assignee_id)
  
  # order
  patent_by_company_april_tbl <- patent_by_company_april_tbl[order(sum_patents, decreasing = TRUE)]
  
  # top 10 
  head(patent_by_company_april_tbl, 10)

# Challenge 3.3
  # Top 10 companies with the most patents
  top_10_tbl <- head(patent_by_company_tbl, 10)

  # Get patents of top 10 companies 
  top_10_patents_tbl <- merge(x = top_10_tbl, y = combi_data, 
                         by    = "assignee_id", 
                         all.x = TRUE, 
                         all.y = FALSE)

  # rearrange
  top_10_patents_tbl <- top_10_patents_tbl %>% select(patent_id, assignee_id)

  # Add the uspc class to each patent_id 
  top_10_patents_tbl <- merge(x = top_10_patents_tbl, y = uspc_tbl, 
                         by    = "patent_id", 
                         all.x = TRUE, 
                         all.y = FALSE)
  
  # most common mainclass_id
  top_mainclass_tbl <- top_10_patents_tbl[!is.na(mainclass_id), .(occurances = .N), by = mainclass_id][order(occurances, decreasing = TRUE)]
  
  # top 5 main classes:
  head(top_mainclass_tbl, 5)

