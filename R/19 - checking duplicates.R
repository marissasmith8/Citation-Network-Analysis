library(tidyverse)
library(googlesheets4)

source("./R/1 - Tidying dataframe to remove duplicates.R")


# All refs coded using google sheets for citation type - deduplicated (to 2311)
sheets_results <- read_sheet("https://docs.google.com/spreadsheets/d/1boszwpnoQ-2stzg396kcM7eh0fibB2oA_voZEVw-rWs/edit#gid=2077854862", sheet = "complete")
sheets_tidied <- sheets_results %>%
  select(Reference,type) %>% 
  unique()

# Conflicts extracted from sheets from shiny app
conflicts_excel <- read_excel("data/sumif_example.xlsx", sheet = "Coi 1203")
conflicts_tidy <- conflicts_excel %>% filter(!is.na(Reference))

# Conflicts in sumif_examples.xlsx which correspond to 2311 full refs
conflicts_tidy %>% inner_join(sheets_tidied, by = c("Reference", "type"))

# Conflicts in sumif_examples.xlsx which have no match in Google sheets
conflicts_tidy %>% select(Reference, type) %>% anti_join(sheets_tidied, by = c("Reference", "type"))

# Journal articles in google sheets which have no match in sumif_examples.xlsx
sheets_tidied %>% filter(type == "Journal article") %>% anti_join(conflicts_tidy, by = "Reference")

# Matching and missing from between sumif_examples.xlsx and Full References excel file
conflicts_tidy %>% inner_join(dfe, by = c("Reference"))
conflicts_tidy %>% select(Reference, type) %>% anti_join(dfe, by = c("Reference"))

# Mismatched in 'Refs full - corrected Jun20.xlsx' excel file/google sheets 'Ongoing Results Screening'
dfe %>% anti_join(sheets_tidied, by = "Reference")
sheets_tidied %>% anti_join(dfe, by = "Reference")


# All references with year listed
sheets_tidied %>% 
  mutate(year = str_extract(Reference, "\\d{4}") %>% 
           as.integer())

# checking with deduplicated sheet
  
deduplicated_conflicts <- read_xlsx("data/corrected_deduplicated.xlsx", sheet = "COI1203")
deduplicated_conflicts %>% left_join(sheets_tidied, by = "Reference") %>% filter(is.na(type.y)) %>% pull(Reference)


deduplicated_conflicts %>% 
  select(Reference, type) %>% 
  anti_join(sheets_tidied, by = c("Reference", "type"))

conflicts_tidy %>% 
  select(Reference, type) %>% 
  anti_join(sheets_tidied, by = c("Reference", "type"))

  sheets_tidied %>% 
  filter(type == "Journal article") %>% 
  anti_join(deduplicated_conflicts, by = c("Reference", "type"))
<<<<<<< HEAD
  
  deduplicated_conflicts %>% filter(type != "Journal article")
  
sheets_tidied %>% 
    group_by(type) %>% 
    count() 
=======


deduplicated_conflicts %>% filter(type != "Journal article") %>% 
  select(Reference, type) %>% 
  left_join(sheets_tidied, by = "Reference")

sheets_tidied %>% 
  group_by(type) %>% 
  count()
>>>>>>> 22a5be82930d9642ea74834aac1d9984dea0fd64
