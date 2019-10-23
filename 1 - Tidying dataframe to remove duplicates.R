library(dplyr)
library(readxl)
library(igraph)
library(purrr)
library(stringr)

#  This script imports the data and creates a dataframe of all refs/reports
#  

# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("Refs full - corrected.xlsx", sheet = "Clean References ")  # import 'Clean References' sheet
# fulltextrefs <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Full References ")  # import 'Full References' sheet

df <- trial1[1:2888, ]

index <- which(is.na(df$Reference))

dffiltered <- df

i <- 0
dfa <- list()

for (p in 1:18) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(dffiltered[i:j, c(1, p+1)])
  i <- index[p]
}

k <- 0
for (k in 1:18) {
  dfa[[k]][[2]] <- 1
  dfa[[k]] <- dfa[[k]][-1,]
}


# dfa now works!
dfb <- dfa %>% map(~ unique(.x))

# creating tidy dataframe ----------------------------------------------------------


dfc <- tibble(Reference = as.character())

q <- 1

for (q in 1:length(dfb)) {
  dfc <- dfc %>% 
    full_join(dfb[[q]], by = "Reference")
}

dfd <- dfc %>% 
  unique() %>% 
  select(-NHS.2017, -ACS.2018) %>% 
  replace(is.na(.), 0)

fill_dataframe <- tibble(nrefs = 1:8,
                         fill = 
                           c("#00a84c"),
                           c("#00ffff"),
                           c("#7f00ff"),
                           c("#dc9dbe"),
                           c("#9d5524"),
                           c("#ff0000"),
                           c("#7fff00"),
                           c("#fe9d00"),
                         )

dfe <- dfd %>% 
  mutate(nrefs = rowSums(.[2:17])) %>% 
  full_join(fill_dataframe, by = "nrefs") %>% 
  filter(Reference != "No references given")

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


# Adding other data -------------------------------------------------------

peco_tab <- read_xlsx("PECO.xlsx")

new_dfe <- peco_tab %>% select(Reference, stud = `Study design`, conf = `Conflict of Interest`) %>% 
  mutate(conf = str_to_sentence(gsub("^(.*)(conclicts)(.*$)", "\\1conflicts\\3", .$conf)),
         stud = str_to_sentence(stud)) %>% 
  full_join(dfe_filtered, by = "Reference") %>% 
  mutate(st_n = as.numeric(as.factor(stud)),
         cn_n = as.numeric(as.factor(conf)))

