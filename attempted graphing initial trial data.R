library(tidyverse)
library(readxl)
library(igraph)



# Import and clean dataset -----------------------------------------------------------------------------------


trial1 <- read_xlsx("draft_refs.xlsx", sheet = "Clean References")  # import 'Clean References' sheet

colnames(trial1) <- colnames(trial1) %>% gsub(" ", "_", .)  # change spaces to '_' in colnames for simplicity

trial2 <- trial1 %>%   # remove rows heading up each document, remove duplicates, fill in 0s for now
  filter(!is.na(WHO_2014)) %>% 
  select(-NHS_2017, -ACS_2018) %>% 
  unique() %>% 
  replace(is.na(.), 0)

row_names <- trial2$Reference  # rownames from column 1

trial_matrix <- trial2 %>%  # rest of columns into matrix
  select(-Reference) %>% 
  as.matrix()

row.names(trial_matrix) <- row_names  # add rownames back in

imatrix <- graph.incidence(trial_matrix, mode = "all")  # igraph matrix
imatrix <- delete.vertices(imatrix, V(imatrix)[degree(imatrix)==0])
V(imatrix)$color[1:1288] <- "#FF0000FF"
V(imatrix)$color[1289:1305] <- "#00FF0080"

V(imatrix)$label[1:1288] <- NA
V(imatrix)$label[1289:1305] <- V(imatrix)$name[1289:1305]
V(imatrix)$size[1:1288] <- 2
V(imatrix)$size[1289:1305] <- 10
V(imatrix)$frame.color <- NA
V(imatrix)$label.cex <- 0.4
V(imatrix)
V(imatrix)



# pdf('imatrix3.pdf')
plot(imatrix, layout = layout.kamada.kawai)
#
