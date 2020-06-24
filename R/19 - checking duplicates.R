library(tidyverse)

conflicts_excel <- read_excel("data/sumif_example.xlsx", sheet = "Coi 1203")
conflicts_tidy <- conflicts_excel %>% filter(!is.na(Reference))

conflicts_tidy %>% inner_join(sheets_tidied, by = c("Reference", "type")) %>% 
  export::table2excel("data/deduplicated_conflicts.xlsx")

conflicts_tidy %>% select(Reference, type) %>% anti_join(sheets_tidied, by = c("Reference", "type"))

conflicts_tidy %>% inner_join(dfe, by = c("Reference"))
conflicts_tidy %>% select(Reference, type) %>% anti_join(dfe, by = c("Reference")) %>%
  export::table2excel("data/in conflicts excel sheet but not in refs full.xlsx")


sheets_tidied %>% 
  mutate(year = str_extract(Reference, "\\d{4}") %>% 
           as.integer()) %>% 
  export::table2excel("data/all refs by type and year.xlsx")
  