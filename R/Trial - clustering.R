library(dendextend)
library(tidyverse)
library(cluster)
library(purrr)


source("./R/1 - Tidying dataframe to remove duplicates.R")

par(lwd=3)

filt_mt <- dfe %>% 
  filter(nrefs >1) %>% 
  select(2:15)

colnames(filt_mt) <- gsub("\\n", " ", colnames(mt_allrefs_so))

mt_byrow <- t(filt_mt)

dist_p <- dist(mt_byrow, "binary")

clust <- hclust(dist_p, method = "average")

plot(clust)

ddg <- as.dendrogram(clust)

ddg <- color_branches(ddg, k = 5)

plot(ddg)

pm_6 <- mt_byrow %>% 
  pam(k = 6)
par()
plot(silhouette(pm_6))

pm_all <- map_dbl(2:10, function(k) {
  model <- pam(x = mt_byrow, k = k)
  model$silinfo$avg.width
})

ggplot(tibble(
  k = 2:10,
  width = pm_all
),
aes(k, width)) + geom_line()


eb_all <- map_dbl(1:10, function(k) {
  kmeans(mt_byrow, centers =  k)$tot.withinss
})


ggplot(tibble(
  k = 1:10,
  tot.ss = eb_all
),
aes(k, tot.ss)) + geom_line() +
  scale_x_continuous(breaks = 1:10)
