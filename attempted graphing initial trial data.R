library(tidyverse)
library(readxl)
library(magrittr)
library(igraph)



# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("draft_refs.xlsx", sheet = "Clean References")  # import 'Clean References' sheet

colnames(trial1) <- colnames(trial1) %>% gsub(" ", "_", .)  # change spaces to '_' in colnames for simplicity

trial2 <- trial1 %>%   # remove rows heading up each document, remove duplicates, fill in 0s for now
  filter(!is.na(WHO_2014)) %>% 
  unique() %>% 
  replace(is.na(.), 0)

row_names <- trial2$Reference  # rownames from column 1

trial_matrix <- trial2 %>%  # rest of columns into matrix
  select(-Reference) %>% 
  as.matrix()

row.names(trial_matrix) <- row_names  # add rownames back in

imatrix <- graph.incidence(trial_matrix, mode = "all")  # igraph matrix
#imatrix <- delete.vertices(imatrix, V(imatrix)[degree(imatrix)==0])
V(imatrix)$color[1:1293] <- "#FF0000FF"
V(imatrix)$color[1294:1311] <- "#00FF0080"

V(imatrix)$label[1:1291] <- NA

V(imatrix)

#pdf('imatrix.pdf')
plot(imatrix, layout = layout.fruchterman.reingold)
#dev.off()
