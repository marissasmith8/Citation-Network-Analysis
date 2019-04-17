library(dplyr)
library(readxl)
library(igraph)



# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("N:/PhD/Methods/Citation Anlaysis/Refs full (08.04.19).xlsx", sheet = "Clean References ")  # import 'Clean References' sheet

df <- trial1[1:2887, ]

index <- which(is.na(df$Reference))

i <- 0
dfa <- list()

for (p in 1:17) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(df[i:j, c(1, p+1)])
  i <- index[p]
}

dfb <- lapply(dfa, na.omit)

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
  mutate(nrefs = rowSums(.[2:17]))

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


matrixe <- dfe %>%
  filter(nrefs >= 3) %>% 
  select(-Reference, -nrefs) %>% 
  as.matrix

rownames(matrixe) <- row_names_dfe