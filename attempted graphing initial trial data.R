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

trial3 <- trial2 %>%  # papers cited by more than 1
  mutate(nrefs = rowSums(.[2:17])) %>% 
  filter(nrefs>1)



# Graphing where more than one citing paper ------------------------------------------------------------------

row_names3 <- trial3$Reference
matrix3 <- trial3 %>% 
  select(-Reference, -nrefs) %>% 
  as.matrix

rownames(matrix3) <- row_names3

imatrix3 <- graph.incidence(matrix3, mode = "all")  # igraph matrix
imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3)

V(imatrix3)$color[1:111] <- "#FF0000FF"
V(imatrix3)$color[112:123] <- "#00FF0080"
V(imatrix3)$size[1:111] <- 2
V(imatrix3)$size[112:123] <- 10
V(imatrix3)$label[1:111] <- NA
V(imatrix3)$label[112:123] <- V(imatrix3)$name[112:123]
V(imatrix3)$frame.color <- NA
V(imatrix3)$label.cex <- 0.4

pdf('filtered.pdf')
plot(imatrix3, layout = layout.fruchterman.reingold)
dev.off()


# Two mode graph ---------------------------------------------------------------------------------------------

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



pdf('imatrix1.pdf')
plot(imatrix, layout = layout.kamada.kawai)
dev.off()


# One mode - documents by connected co-citations -------------------------------------------------------------

oneModeMatrix <- t(trial_matrix) %*% trial_matrix

oneImatrix <- graph.adjacency(oneModeMatrix, mode = "undirected")

E(oneImatrix)$weight <- count.multiple(oneImatrix)
oneImatrix <- simplify(oneImatrix)
oneImatrix <- delete.vertices(oneImatrix, V(oneImatrix)[degree(oneImatrix)==0])

V(oneImatrix)$frame.color <- NA
V(oneImatrix)$size <- 6
V(oneImatrix)$color <- "#0000FF80"
V(oneImatrix)$label.color <- "#000000FF"
V(oneImatrix)$label.cex <- 0.6

egam <- (log(E(oneImatrix)$weight)+.3)/max(log(E(oneImatrix)$weight)+.3)
E(oneImatrix)$color <- rgb(0.8,0.2,0,egam)

pdf('one.pdf')
plot(oneImatrix, layout = layout.fruchterman.reingold)
dev.off()
