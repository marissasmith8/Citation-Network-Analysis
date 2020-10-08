load("data/biSBM_points.rda")

library(tidyverse)

key_points <- tibble(x = c(3, 4, 5, 6),
                     y = c(3, 4, 4, 6))

key_models<-points_repeat %>% 
  semi_join(key_points, by=c("x", "y"))

model<- clust1
coi_google <- read_sheet("https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4/edit#gid=64883021")

#Extract gluster analysis function
#extract_clust<- function(model,coi_google=coi_google) {

glclusts<-tibble(gldocs=colnames(matrix_e), 
                 g_clust=model$groups[193:206])


refs_by_cluster<-mutate(df_filter, r_clust=model$groups[1:192]) %>% 
  select(-nr_fill, -nrefs) %>% 
  pivot_longer(-c(Reference, r_clust),names_to = "gldocs", values_to = "cited") %>% 
  left_join(glclusts, by="gldocs") %>% 
  select(Reference, gldocs, r_clust, g_clust, cited)

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
  ref_coi_prop=ncoitabdf_r_prop,
  ref_coi_chi=chi_ref_groups)
#}


extract_clust(model=clust1)
