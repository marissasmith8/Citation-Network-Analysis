# **filtering and adding other data -------------------------------------------

dfe_filtered <- dfe_ordered %>% filter(nrefs>2)

# peco_tab <- read_xlsx("./data/PECO (23.10.19).xlsx")
peco_tab <- readxl::read_xlsx("./data/PECO.xlsx")  # updated version

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
  select(2:17) %>%
  as.matrix

row.names(mt_allrefs_so) <- row_names_dfe_ordered

colnames(mt_allrefs_so) <- reports_formatted
#### mt_allrefs_so - output matrix for igraph of all points


# Filtered matrix for igraphs 2-4 -----------------------------------------

mt_fl_so <- full_dfe %>% 
  select(4:19) %>% 
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
                                        rep("AUS", 4),
                                        rep("USA", 3)))) %>% 
  full_join(ccols, by = "context")



