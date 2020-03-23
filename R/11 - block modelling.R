library(tidyverse)
source("./R/1 - Tidying dataframe to remove duplicates.R")


# processing --------------------------------------------------------------

doc_ids <- dfe %>% select(Reference) %>% 
  arrange(Reference) %>% 
  mutate(id = paste0("#", as.numeric(as.factor(Reference))))

# ** matrix transposed and grouped ----------------------------------------

docs_numbers <- dfb %>% map_df(~ mutate(.x, doc = colnames(.x)[2]) %>% 
                 group_by(doc) %>% 
                 count()) %>% 
  bind_cols(tibble(context = c(rep("WHO", 2),
                                         rep("UK", 8),
                                         rep("AUS", 4),
                                         rep("USA", 4)))) %>% 
  ungroup() %>% 
  mutate(doc = gsub("\\.", " ", .$doc))

matrix2 <-   dfe %>% 
  left_join(doc_ids, by = "Reference") %>% 
  select(-Reference) %>% 
  filter(nrefs > 1) %>% 
    select(-nrefs, -nr_fill) %>%
    column_to_rownames("id")  %>%
  t() 

matrix3 <-   dfe %>% 
  left_join(doc_ids, by = "Reference") %>% 
  select(-Reference) %>% 
  filter(nrefs > 2) %>% 
    select(-nrefs, -nr_fill) %>%
    column_to_rownames("id")  %>%
  t() 

rownames(matrix2) <- gsub("\\.", " ", rownames(matrix2))
rownames(matrix3) <- gsub("\\.", " ", rownames(matrix3))

gls_by_doc <- matrix2 %>% 
  as_tibble() %>%
  mutate(doc = gsub("\\n", " ", colnames(mt_allrefs_so))) %>%
  left_join(docs_numbers, by = "doc") %>%
  select(doc, context, 1:300)


mt_docs <- gls_by_doc %>% 
  group_by(context) %>% 
  summarise_if(is.numeric, sum) %>% 
  select(-context) %>% 
  data.matrix()

# ** setting up data ------------------------------------------------------

mt <- mt_allrefs_so
colnames(mt) <-  gsub("\n", " ", colnames(mt_allrefs_so))

# Trying bipartite computemodules -----------------------------------------

library(bipartite)

rownames(doc_ids) <- doc_ids$id
colnames(matrix3) <- doc_ids[colnames(matrix3),"Reference", drop = TRUE]

matrixb <- t(matrix3)

modulesb <- computeModules(matrix3, method = "DormannStrauss", deep = TRUE)

plotModuleWeb(modulesb, labsize = 0.5)


listModuleInformation(modulesb)[[2]] %>% 
  map2_df(1:length(.), ~ tibble(Cluster = .y, `Guideline Documents` = str_flatten(.x[[1]], ", "), `Number of references in cluster` = length(.x[[2]]))) %>% 
  mutate_all(as.character) %>% 
  export::table2doc("outputs/Clusters_filtered2.docx")

ggsave(plot = plotModuleWeb(modulesb), filename = "graphs/clusters_filtered2.svg", units = "mm", height = 1200, width = 800)

# ** whole n>1 matrix -----------------------------------------------------

matrixc <- t(matrix2)
colnames(matrix2) <- doc_ids[colnames(matrix2),"Reference", drop = TRUE]

modulesc <- computeModules(matrix2, method = "Beckett")

listModuleInformation(modulesc)[[2]] %>% 
  map2_df(1:length(.), ~ tibble(Cluster = .y, `Guideline Documents` = str_flatten(.x[[1]], ", "), `Number of references in cluster` = length(.x[[2]]))) %>% 
  mutate_all(as.character) %>% 
  export::table2doc("outputs/Clusters_filtered1.docx")

plotModuleWeb(modulesc, labsize = 0.2)


# matrix without deep -----------------------------------------------------

modules3 <- computeModules(mt)
modules3b <- computeModules(t(mt), method = "Beckett")
mod3_layout <- listModuleInformation(modules3b)

mod3_layout[[2]] %>% 
  map2_df(1:12, ~ tibble(Cluster = .y, `Guideline Documents` = str_flatten(.x[[1]], ", "), `Number of references in cluster` = length(.x[[2]]))) %>% 
  mutate_all(as.character) # %>% 
  export::table2doc("Clusters.docx")


save(mt, file = "data/all_citations_matrix.rda")
# save(modulesb, modulesc, modules3b, file = "data/clusters.rda")
