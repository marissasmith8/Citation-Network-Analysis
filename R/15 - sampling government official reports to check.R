library(tidyverse)

load("data/googlesheets_results.rda")
source("./R/1 - Tidying dataframe to remove duplicates.R")

govt_reps <- sheets_results %>% 
  filter(type == "Government/official report") %>% 
  select(Reference, url, title) %>% 
  left_join(dfe, by = "Reference")

order <- govt_reps %>%
  summarise_at(4:19, ~sum(.x, na.rm = TRUE))  %>% 
  pivot_longer(1:16, names_to = "doc", values_to = "n") %>% 
  arrange(n) %>% 
  pull(doc)


selections <- order %>% map(~ govt_reps %>% 
              select(Reference, .x, nrefs) %>% 
                filter_at(2, ~.x == 1) %>% 
                arrange(desc(nrefs)))

govt_temp <- govt_reps
out <- list()

for (i in 1:length(selections)) {
  df <- semi_join(govt_temp, selections[[i]], by = "Reference")
  
  df <- head(df, 4)
  
  govt_temp <- govt_temp %>% 
    anti_join(df, by = "Reference")
  
  out[[i]] <- df
}

out %>% View()


names(out) <- order

out$WHO.2014

map2_dfr(order, out, ~ mutate(.y, gl = .x) %>% 
           select(Reference, url, title, gl, nrefs)) %>% 
  write_csv("Government documents to search.csv")
