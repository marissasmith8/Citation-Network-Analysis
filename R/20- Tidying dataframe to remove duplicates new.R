library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(ggplot2)
library(SPHSUgraphs)

#  This script imports the data and creates a dataframe of all refs/reports
#  

# Import and clean dataset -----------------------------------------------------------------------------------

import <- readxl::read_xlsx("./data/Refs full - corrected Jul20.xlsx", sheet = "Clean References")  # import 'Clean References' sheet
# import <- readxl::read_xlsx("./data/Refs full - corrected Jul20.xlsx", sheet = "Clean References")  # import 'Clean References' sheet

# Relevant rows
df <- import[1:2032, ] # all rows now relevant

index <- which(is.na(df$Reference))

dffiltered <- df

# create list of each report's citations in dataframe with column of 1s
i <- 0
dfa <- list()

for (p in 1:15) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(dffiltered[i:j, c(1, p+1)])
  i <- index[p]
}

k <- 0
for (k in 1:length(dfa)) {
  dfa[[k]][[2]] <- 1
  dfa[[k]] <- dfa[[k]][-1,]
}


# Remove duplicate rows in each dataframe
dfb <- dfa %>% map(~ unique(.x))

# **creating tidy dataframe ----------------------------------------------------------

dfc <- dfb %>% reduce(full_join, by = "Reference")

# de-duplicate rows, remove columns with 0 ref papers
dfd <- dfc %>% 
  unique() %>% 
  select(-NHS.2017) %>% 
  replace(is.na(.), 0)


# **Dataframe with fill colours by nrefs ------------------------------------
## CREATE DF FOR STUD AND CONF
pres_fill = c("#00a84c",
              "#00ffff",
              "#7f00ff",
              "#dc9dbe",
              "#9d5524",
              "#ff0000",
              "#7fff00",
              "#fe9d00")

# paper_fill <- sphsu_cols("leaf",
#                          "turquoise",
#                          "Rose",
#                          "university blue",
#                          "Rust",
#                          "Thistle", 
#                          "Moss",
#                          "pumpkin",
#                          names = FALSE)

cont_fill <- scales::brewer_pal(palette = 'Spectral')(6) %>% 
  rev() %>% 
  c(., rep('#blank', 3))

paper_fill <- c("#00843D",
                "#00B5D1", 
                "#B06C96",
                "#003865", 
                "#BE4D00",
                "#951272", 
                "#11DD11",
                "#FFB948",
                "#EE11EE")


fill_dataframe_pres <- 
  tibble(conf = c("No mention", "Declared none", "Pharmaceutical", 
                  "Both e-cigarrette and pharmaceutical", "E-cigarette", "Tobacco company", 
                  "Tobacco control advocate", NA),
         cn_n = c(4, 3, 6, 1, 2, 7, 5, 8),
         cn_fill = c("#dc9dbe", "#7f00ff", "#ff0000", "#00a84c", "#00ffff", "#7fff00", "#9d5524", "#fe9d00")) %>% 
  bind_cols(tibble(nrefs = 1:8,
                   st_n = 1:8,
                   nr_fill = pres_fill,
                   st_fill = pres_fill
  ))

fill_dataframe_paper <- tibble(nrefs = 1:9,
                               st_n = 1:9,
                               conf = c("No mention", "Declared none", "Pharmaceutical", 
                                        "Both e-cigarrette and pharmaceutical", "E-cigarette", "Tobacco company", 
                                        "Tobacco control advocate", NA, NA),
                               cn_n = c(4, 3, 6, 1, 2, 7, 5, 8, 9),
                               # cn_n = 1:8,
                               nr_fill = cont_fill,
                               st_fill = paper_fill,
                               cn_fill = paper_fill
)

# Edit here for colour scheme change
dfe <- dfd %>% 
  mutate(nrefs = rowSums(.[2:15])) %>% 
  full_join(fill_dataframe_paper %>% select(nrefs, nr_fill), by = "nrefs") %>%  # fill_dataframe_[pres/paper]
  # full_join(fill_dataframe_pres %>% select(nrefs, nr_fill), by = "nrefs") %>%  # fill_dataframe_[pres/paper]
  filter(Reference != "No references given")

dfe_ordered <- dfe %>% arrange(nrefs)

row_names_dfe_ordered <- dfe_ordered %>% pull(Reference)

# **Formatting Guideline doc names ----------------------------------------------

reports <- colnames(dfd %>% select(-(Reference)))
reports_formatted <- gsub("\\.(\\d{4}$)", "\n\\1", reports)  # adding line breaks
reports_formatted <- gsub("\\.", " ", reports_formatted)  # adding space in one instance


# **filtering and adding other data -------------------------------------------

dfe_filtered <- dfe_ordered %>% filter(nrefs>2)

# peco_tab <- read_xlsx("./data/PECO (23.10.19).xlsx")
peco_tab <- readxl::read_xlsx("./data/PECO JUL 20.xlsx")  # updated version

full_dfe <-  peco_tab %>%
  select(Reference, stud = `Study design`, conf = `Conflict of Interest`) %>% 
  mutate(conf = str_to_sentence(gsub("^(.*)(conclicts)(.*$)", "\\1conflicts\\3", .$conf)),
         stud = ifelse(stud=="RCT", stud, str_to_sentence(stud)),
         stud = str_replace(stud, "carlo", "Carlo"),
         stud = ifelse(grepl("^[Ll]ong.*$", .$stud), "Longitudinal study", stud),
         stud = ifelse(grepl("^[Pp]olicy.*$", .$stud), "Policy document", stud),
         stud = ifelse(grepl("^[Cc]ross.*$", .$stud), "Cross-sectional", stud),
         stud = ifelse(grepl("^[Ss]ystematic.*$", .$stud), "Systematic Review", stud),
         conf = ifelse(grepl("^Pharam.*$", .$conf), "Pharmaceutical", conf),
         conf = ifelse(grepl("^[Oo]utlier.*$", .$conf), "Tobacco control advocate", conf),
         conf = ifelse(grepl("^[Nn]o conflicts of intere.*$", .$conf), "Declared none", conf),
         conf = ifelse(grepl("^[Nn]o confl.*stated$", .$conf), "No mention", conf)
         # conf = ifelse(grepl("^[Nn]o confl*$", .$conf), "Declared none", conf)
  ) %>% 
  # pull(conf) %>% unique()  # for testing duplicates
  full_join(dfe_filtered, by = "Reference") %>% 
  mutate(st_n = as.numeric(as.factor(stud))) %>% 
  full_join(fill_dataframe_paper %>% select(st_n, st_fill), by = "st_n") %>% 
  full_join(fill_dataframe_paper %>% select(conf, cn_n, cn_fill), by = "conf") %>% 
  filter(!is.na(Reference)) %>% 
  arrange(desc(nrefs)) %>% 
  mutate(id = 1:nrow(.))

# Ordered matrix for igraph1 ----------------------------------------------


mt_allrefs_so <- dfe_ordered %>%
  select(-c(Reference, nrefs, nr_fill)) %>%
  as.matrix

row.names(mt_allrefs_so) <- row_names_dfe_ordered

colnames(mt_allrefs_so) <- reports_formatted
#### mt_allrefs_so - output matrix for igraph of all points


# Filtered matrix for igraphs 2-4 -----------------------------------------

mt_fl_so <- full_dfe %>% 
  select(4:17) %>%
  # select(-Reference, -nrefs, -fill, -stud, -st_n, -conf, -cn_n) %>%
  as.matrix()

row.names(mt_fl_so) <- full_dfe$Reference
colnames(mt_fl_so) <- reports_formatted


# Guideline colour legend ------------------------------------------------------------------------------------


ccols <- tibble(context =  as.factor(c("WHO",
                                       "UK",
                                       "AUS",
                                       "USA")),
                fill = c("#d3e585",
                         "#ffe771",
                         "#a6a6a6",
                         "#e3aa84"))

gl_cols <- tibble(Report = reports_formatted,
                  context = as.factor(c(rep("WHO", 2),
                                        rep("UK", 7),
                                        rep("AUS", 2),
                                        rep("USA", 3)))) %>% 
  full_join(ccols, by = "context")
