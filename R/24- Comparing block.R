# load("data/biSBM_points.rda")
source("C:/Users/marissa/Documents/Citation-Network-Analysis-New/Citation-Network-Analysis-New/R/biSBMR/biSBMWin.R")
library(tidyverse)
library(igraph)
library(googlesheets4)
source("./R/20- Tidying dataframe to remove duplicates new.R")


# re-writing the dataframe ------------------------------------------------

df_filter <- filter(dfe, nrefs >1)
matrixe <- df_filter %>%
  select(2:15) %>%
  as.matrix() %>% 
  `rownames<-`(df_filter$Reference)

p <- graph.incidence(matrixe)

adj <- get.adjacency(p, sparse = FALSE)


# choosing the ones to put in ---------------------------------------------

key_points <- tibble(x = c(3, 4, 5, 6),
                     y = c(3, 4, 4, 6))

# redoing all clustering with 50 iterations to make sure we have the best of them all!

set.seed(123)  # remember this ensures we get consistent modelling each time

nodeType <- c(rep(1, nrow(df_filter)), rep(2, 14))
key_models <- key_points %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 50))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))



coi_google <- read_sheet("https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4/edit#gid=64883021")



#Extract gluster analysis function
extract_clust<- function(groups, reference_sheet=coi_google) {

glclusts<-tibble(gldocs=c("WHO.2014", "WHO.2016", "PHE.2015", "PHE.2016", "PHE.2018", 
                          "PHE.2019", "NICE.2018", "PHS.2016", "PHW.2017", "NHMRC.2017", 
                          "PHAA.2018", "SGR.2016", "FDA.2016", "APHA.2018"), 
                 g_clust=tail(groups, 14))


refs_by_cluster2<-mutate(df_filter, r_clust=groups[1:nrow(df_filter)]) %>% 
  select(-nr_fill, -nrefs) %>% 
  pivot_longer(-c(Reference, r_clust),names_to = "gldocs", values_to = "cited") %>% 
  left_join(glclusts, by="gldocs") %>% 
  select(Reference, gldocs, r_clust, g_clust, cited)

coidf<-inner_join(refs_by_cluster2, reference_sheet, by = "Reference")
# View(coidf)
coiu<-coidf %>% 
  filter(cited==1) %>% 
  select("Reference", "r_clust","g_clust", "no mention":"tobacco control advocate") %>% unique()


#Grouping by cluster
coitabdf<-coiu %>% group_by(g_clust) %>% summarise_at(vars(`no mention`:`tobacco control advocate`), ~sum(.x,na.rm = TRUE))


#Create table of coi by type
coitydf<-
  coitabdf %>% 
  pivot_longer(-g_clust,names_to="coi_type", values_to="n") %>% 
  group_by(g_clust) %>% 
  mutate(prop=n/sum(n))

#chi-square for guideline cluster groups
chi_gl_groups<-coitydf %>% 
  select(-prop) %>% 
  pivot_wider(names_from = "g_clust", values_from = "n") %>% 
  select(-coi_type) %>% 
  as.matrix() %>% 
  chisq.test()

#chi-square for reference cluster groups
coitabdf_r<-coiu %>% group_by(r_clust) %>% summarise_at(vars(`no mention`:`tobacco control advocate`), ~sum(.x,na.rm = TRUE))


#Create table of coi by type
coitydf_r<-
  coitabdf_r %>% 
  pivot_longer(-r_clust,names_to="coi_type", values_to="n") %>% 
  group_by(r_clust) %>% 
  mutate(prop=n/sum(n))

#chi-square for reference cluster groups
chi_ref_groups<-coitydf_r %>% 
  select(-prop) %>% 
  pivot_wider(names_from = "r_clust", values_from = "n") %>% 
  select(-coi_type) %>% 
  as.matrix() %>% 
  chisq.test()

#Table of proportion of gl documents
coitabdf
coitabdf_prop<-coitydf %>% 
  select(-n) %>% 
  mutate(prop=prop*100) %>% 
  pivot_wider(names_from = "coi_type", values_from="prop")

#Table of proportion of ref documents 
coitabdf_r_prop<-coitydf_r %>% 
  select(-n) %>% 
  mutate(prop=prop*100) %>% 
  pivot_wider(names_from = "coi_type", values_from="prop")

list(
  gl_coi_n=coitabdf, 
  gl_coi_prop=coitabdf_prop,
  gl_coi_chi=chi_gl_groups,
  ref_coi_n=coitabdf_r,
  ref_coi_prop=coitabdf_r_prop,
  ref_coi_chi=chi_ref_groups)
}



# function now works!!! ---------------------------------------------------

all_blocks <- key_models %>% 
  mutate(blocks = map(group_model, ~extract_clust(.x$groups))) %>% 
  unnest_wider(blocks) %>% 
  rowwise() %>% 
  mutate_at(vars(gl_coi_chi, ref_coi_chi), 
                list(p_val = ~ .x$p.value,
                     X_squared = ~ .x$statistic,
                     df = ~ .x$parameter
                     ),
                .names="{.col}_{.fn}")

# This dataframe created above now has the results of all the chi-squared tests and the tables of distributions


# here's a table of the chi-squared test results (X-squared statistic, df and p value)
# This could be a good summary table of all sensitivity tests?

all_blocks %>% 
  select(
    `Number of Guideline document blocks` = y,
    `Number of Reference blocks` = x,
    `log-likelihood score` = score,
    `Gl docs clusters X-squared` = gl_coi_chi_X_squared,
    `Gl docs clusters df` = gl_coi_chi_df,
    `Gl docs clusters p-value` = gl_coi_chi_p_val,
    `Reference clusters X-squared` = ref_coi_chi_X_squared,
    `Reference clusters df` = ref_coi_chi_df,
    `Reference clusters p-value` = ref_coi_chi_p_val,
  ) %>% 
  View()


# To look at a table you can access it like this:

all_blocks %>% 
  filter(x == 5, y == 4) %>%   # Choose the clusters to filter for
  pull(gl_coi_chi)            # choose which value to check out
