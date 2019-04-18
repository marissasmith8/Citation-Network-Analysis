library(dplyr)
library(readxl)
library(igraph)



# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("N:/PhD/Methods/Citation Anlaysis/Refs full (08.04.19).xlsx", sheet = "Clean References ")  # import 'Clean References' sheet
fulltextrefs <- read_xlsx("N:/PhD/Methods/Citation Anlaysis/Refs full (08.04.19).xlsx", sheet = "Full References ")  # import 'Full References' sheet

df <- trial1[1:2886, ]

index <- which(is.na(df$Reference))

i <- 0
dfa <- list()

dffiltered <- select(df, -`ACS 2018`)


for (p in 1:17) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(dffiltered[i:j, c(1, p+1)])
  i <- index[p]
}

dfb <- lapply(dfa, na.omit)

dfbr <- dfb[c(3:17, 1:2)]

# Original order ----------------------------------------------------------


dfc <- tibble(Reference = "x")

q <- 1

for (q in 1:length(dfb)) {
  dfc <- dfc %>% 
    full_join(dfb[[q]], by = "Reference")
}

dfd <- dfc %>% 
  unique() %>% 
  filter(Reference != "x") %>% 
  replace(is.na(.), 0)

dfe <- dfd %>% 
  filter(NHS.2017 == 0) %>% 
  select(-NHS.2017) %>% 
  mutate(nrefs = rowSums(.[2:17])) %>% 
  mutate(colour = ifelse(nrefs == 1, sphsu_cols("leaf"),
                         ifelse(nrefs == 2, sphsu_cols("turquoise"),
                         sphsu_cols("thistle"))))

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


# Reordered dataset -------------------------------------------------------


dfcr <- tibble(Reference = "x")

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
  mutate(nrefs = rowSums(.[2:17])) %>% 
  mutate(colour = ifelse(nrefs == 1, sphsu_cols("leaf"),
                         ifelse(nrefs == 2, sphsu_cols("turquoise"),
                                sphsu_cols("thistle"))))

row_names_dfer <- dfer %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


# Top cited ---------------------------------------------------------------

dfb
