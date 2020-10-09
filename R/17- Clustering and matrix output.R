source("C:/Users/marissa/Documents/Citation-Network-Analysis-New/Citation-Network-Analysis-New/R/biSBMR/biSBMWin.R")
# source("R/biSBMR/biSBMWin.R")
source("./R/20- Tidying dataframe to remove duplicates new.R")

library(tidyverse)
library(igraph)
library(export)
df_filter <-dfe %>% filter(nrefs >1)
matrix_e <- df_filter %>% select(-Reference, -nr_fill,-nrefs) %>% as.matrix()
row.names(matrix_e) <- df_filter[["Reference"]]

adj <- get.adjacency(graph.incidence(matrix_e), sparse = FALSE)
nodesType <- c(rep(1,192),rep(2,14))

set.seed(5)

clust1 <-biSBM(adj,nodeType = nodesType, ka=5,kb=4, iter=50)
#points_repeat %>%  filter(x==5, y==4) %>% .$group_model %>% .[[1]]
colnames(matrix_e)
glclusts<-tibble(gldocs=colnames(matrix_e), 
                 g_clust=clust1$groups[193:206])


refs_by_cluster<-mutate(df_filter, r_clust=clust1$groups[1:192]) %>% 
 select(-nr_fill, -nrefs) %>% 
  pivot_longer(-c(Reference, r_clust),names_to = "gldocs", values_to = "cited") %>% 
  left_join(glclusts, by="gldocs") %>% 
  select(Reference, gldocs, r_clust, g_clust, cited)

#Counting nrefs and ngls
nrefs_by_block <- refs_by_cluster %>%  select(Reference, r_clust) %>% unique() %>% group_by(r_clust) %>% count() %>% ungroup() 


# creating line break
refs_lines <-nrefs_by_block %>% mutate(n = cumsum(n)) %>%  pull(n) +0.5
gl_lines <-refs_by_cluster %>%  select(gldocs, g_clust) %>% unique() %>% group_by(g_clust) %>% count() %>% ungroup() %>% mutate(n = cumsum(n)) %>%  pull(n) +0.5


refs_by_cluster %>% mutate(Reference=reorder(Reference, r_clust), gldocs=reorder(gldocs,g_clust), cited=factor(cited)) %>%
  ggplot(aes(Reference, gldocs, fill= cited)) +geom_tile() +
  scale_fill_manual(values = c("white", "black"))+ 
  geom_hline(yintercept=gl_lines) +
  geom_vline(xintercept = refs_lines) + 
  scale_y_discrete(expand = c(0,0)) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave(file = "graphs/blockmodelling.svg", width=300/25, height = 150/25)

graph2ppt(last_plot(), "blockmodelling.pptx", append=TRUE)
          