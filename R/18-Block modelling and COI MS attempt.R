library(dplyr)
library(googlesheets4)
library(tidyr)

#Joining block model cluster data frame to coi dataframe

coi_google <- read_sheet("https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4/edit#gid=64883021")
coidf<-inner_join(refs_by_cluster, coi_google, by = "Reference")
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

coitydf %>% 
  select(-prop) %>% 
  pivot_wider(names_from = "g_clust", values_from = "n") %>% 
  select(-coi_type) %>% 
  as.matrix() %>% 
  chisq.test()


# on all refs -------------------------------------------------------------

cois_by_all_refs <- refs_by_cluster %>% 
  select(gldocs, g_clust) %>% 
  unique() %>% 
  group_by(g_clust) %>% 
  nest() %>% 
  transmute(gldocs = map(data, ~pull(.x, gldocs))) %>% 
  mutate(citations = map(gldocs,  ~ dfe[c("Reference", .x)] %>% 
                           filter_at(-1, any_vars(.==1)) %>% 
                           inner_join(coi_google, by = "Reference") %>% 
                           select("no mention":"tobacco control advocate") %>%
                           mutate(total = 1) %>%
                           summarise_all(~sum(.x, na.rm = TRUE))
                         ),
         gldocs = str_flatten(gldocs[[1]], collapse = ", ")
         ) %>%
  # select(-data) %>% 
  unnest(citations) %>% 
  arrange(g_clust) %>% 
  # mutate(none = `no mention` + `none declared`,
  #        `no mention` = NULL,
  #        `none declared` = NULL) %>% 
  ungroup() 

cois_by_all_refs %>% 
  select(-g_clust, -gldocs, -total) %>% 
  as.matrix() %>% 
  chisq.test()
