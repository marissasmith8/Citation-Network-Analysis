library(dplyr)
library(readxl)
library(igraph)
library(purrr)


# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Clean References ")  # import 'Clean References' sheet
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

dfb <- lapply(dfa, na.omit)

# dfbr <- dfb[c(3:17, 1:2)]

# Original order ----------------------------------------------------------


dfc <- tibble(Reference = as.character())

q <- 1

for (q in 1:length(dfb)) {
  dfc <- dfc %>% 
    full_join(dfb[[q]], by = "Reference")
}

dfd <- dfc %>% 
  unique() %>% 
  select(-NHS.2017) %>% 
  replace(is.na(.), 0)

dfe <- dfd %>% 
  mutate(nrefs = rowSums(.[2:17])) %>% 
  full_join(fill_dataframe, by = "nrefs")

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


# Reordered dataset -------------------------------------------------------


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




# Adding column of paper referencing -------------------------------------------------------------------------

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
