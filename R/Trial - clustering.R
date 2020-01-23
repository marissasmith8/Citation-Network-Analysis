library(dendextend)
library(tidyverse)

source("./R/1 - Tidying dataframe to remove duplicates.R")


filt_mt <- dfe %>% 
  filter(nrefs >1) %>% 
  select(2:17)

colnames(filt_mt) <- gsub("\\.", " ", colnames(mt_allrefs_so))

mt_byrow <- t(filt_mt)

dist_p <- dist(mt_byrow, "binary")

clust <- hclust(dist_p, method = "average")

plot(clust)

ddg <- as.dendrogram(clust)

ddg <- color_branches(ddg, k = 6)

plot(ddg)
