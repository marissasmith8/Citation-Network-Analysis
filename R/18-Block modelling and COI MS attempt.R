library(dplyr)
library(googlesheets4)
library(tidyr)

#Joining block model cluster data frame to coi dataframe

coi_google <- read_sheet("https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4/edit#gid=64883021")
coidf<-inner_join(refs_by_cluster, coi_google, by= "Reference")
view(coidf)
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
