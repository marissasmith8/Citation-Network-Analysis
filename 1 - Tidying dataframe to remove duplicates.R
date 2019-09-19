library(dplyr)
library(readxl)
library(igraph)

#  This script imports the data and creates a dataframe of all refs/reports
#  

# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("Refs full - corrected.xlsx", sheet = "Clean References ")  # import 'Clean References' sheet
# fulltextrefs <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Full References ")  # import 'Full References' sheet

df <- trial1[1:2887, ]

index <- which(is.na(df$Reference))

dffiltered <- select(df, -`ACS 2018`)

i <- 0
dfa <- list()

for (p in 1:17) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(dffiltered[i:j, c(1, p+1)])
  i <- index[p]
}

k <- 0
for (k in 1:17) {
  dfa[[k]][[2]] <- 1
  dfa[[k]] <- dfa[[k]][-1,]
}


# dfa now works!

# Original order ----------------------------------------------------------


dfc <- tibble(Reference = as.character())

q <- 1

for (q in 1:length(dfb)) {
  dfc <- dfc %>% 
    full_join(dfa[[q]], by = "Reference")
}

dfd <- dfc %>% 
  unique() %>% 
  select(-NHS.2017) %>% 
  replace(is.na(.), 0)

dfe <- dfd %>% 
  mutate(nrefs = rowSums(.[2:17])) %>% 
  full_join(fill_dataframe, by = "nrefs") %>% 
  filter(Reference != "No references given")

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)



# detect approximate duplicates - not used ------------------------------------------------------------------------------
possibleDups <- list()

for (l in 1:(nrow(dfe)-1)) {

  dups <- agrep(dfe[[l, "Reference"]], dfe$Reference[l:nrow(dfe)], value = TRUE)
  dups <- dups[grepl(gsub("^.*(\\(.*\\))$", "\\1", dfe[[l, "Reference"]]), gsub("^.*(\\(.*\\))$", "\\1", dups), dups, fixed = TRUE)]
  dups <- dups[grepl(gsub("^(\\D).*$", "\\1", dfe[[l, "Reference"]]), gsub("^(\\D).*$", "\\1", dups), dups, fixed = TRUE)]

  if(length(dups) > 1){
    possibleDups[[l]] <- dups
  } else {
    possibleDups[[l]] <- NA
  }
}

tidyDups <- discard(possibleDups, anyNA)

output_duplicates <- tidyDups %>% map(function(x) tibble(a = 1:length(x), b = x) %>% tidyr::spread(a,b)) %>% 
  reduce(bind_rows)

write.csv(output_duplicates, file = "output duplicates.csv", row.names = FALSE)

# Reordered dataset - not used -------------------------------------------------------


dfcr <- tibble(Reference = as.character())

q <- 1

for (q in 1:length(dfb)) {
  dfcr <- dfcr %>% 
    full_join(dfbr[[q]], by = "Reference")
}

dfdr <- dfcr %>% 
  unique() %>% 
  filter(Reference != "x") %>% 
  replace(is.na(.), 0)

dfer <- dfdr %>% 
  filter(NHS.2017 == 0) %>% 
  select(-NHS.2017) %>% 
  mutate(nrefs = rowSums(.[2:17]))  %>% 
  full_join(fill_dataframe, by = "nrefs")

row_names_dfer <- dfer %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)




# Adding column of paper referencing - not used -------------------------------------------------------------------------

dfb2 <- lapply(dfb, function(x) mutate(x,Paper = names(x)[2]))

df_originalrefs <- purrr::reduce(dfb2, bind_rows) %>% select(Reference, Paper)

dfd %>% 
  mutate(nrefs = rowSums(.[2:18])) %>% 
  full_join(df_originalrefs, by = "Reference") %>% 
  filter(NHS.2017 == 0) %>% 
  select(Paper, Reference, nrefs) %>%
  group_by(Paper) %>% 
  top_n(3)  %>% # To show top 3 cited papers in each
  arrange(Paper, desc(nrefs))
