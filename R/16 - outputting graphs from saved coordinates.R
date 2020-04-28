library(igraph)
library(tidyverse)
library(ggraph)
library(svglite)

load("./data/coords.rdata")
source("./R/1 - Tidying dataframe to remove duplicates.R")

#  In this file, the preformed layouts are loaded
#  Each graph's attributes can be edited using the `V` and `E` functions
#  outputs will be svg files

# Graph 1 - all refs, unfiltered ------------------------------------------


svglite("./graphs/igraph - all refs - coloured - no groups - paper.svg", width = 300/25, height = 150/25)
plot(ig_allrefs_so,
     # mark.groups = groups,  # group bubbles
     layout = co_ig1,
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig1[,1])),
     ylim = c(0,max(co_ig1[,2])),
)
dev.off()



# Graph 2 - filtered, coloured by nrefs -----------------------------------

svglite("./graphs/igraph - filtered - coloured - paper.svg", width = 300/25, height = 150/25)
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = co_ig2, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig2[,1])),
     ylim = c(0,max(co_ig2[,2])),
)
dev.off()


# Graph 3 - coloured by conflicts -----------------------------------------

V(ig_fl_so)$color[refs3] <- full_dfe$cn_fill


svglite("./graphs/igraph - filtered - col_conf - paper.svg", width = 300/25, height = 150/25)
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = co_ig2, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig2[,1])),
     ylim = c(0,max(co_ig2[,2])),
)
dev.off()


# Graph 4 - coloured by study type ----------------------------------------

V(ig_fl_so)$color[refs3] <- full_dfe$st_fill


svglite("./graphs/igraph - filtered - col_stud - paper.svg", width = 300/25, height = 150/25)
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = co_ig2, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig2[,1])),
     ylim = c(0,max(co_ig2[,2])),
)
dev.off()
