library(dplyr)
library(readxl)
library(igraph)
library(SPHSUgraphs)



# Import and clean dataset -----------------------------------------------------------------------------------


# trial1 <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Clean References ")  # import 'Clean References' sheet
# 
# colnames(trial1) <- colnames(trial1) %>% gsub(" ", "_", .)  # change spaces to '_' in colnames for simplicity
# 
# trial2 <- trial1 %>%   # remove rows heading up each document, remove duplicates, fill in 0s for now
#   filter(!is.na(WHO_2014)) %>% 
#   select(-NHS_2017, -ACS_2018) %>% 
#   unique() %>% 
#   replace(is.na(.), 0)
# 
# trial3 <- trial2 %>%  # papers cited by more than 1
#   mutate(nrefs = rowSums(.[2:17])) %>% 
#   filter(nrefs>1)



# New dataframe manipulation ---------------------------------------------------------------------------------

dfe <- dfd %>% 
  filter(NHS.2017 == 0) %>% 
  select(-NHS.2017) %>% 
  mutate(nrefs = rowSums(.[2:17]))

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)
  

matrixe <- dfe %>%
  filter(nrefs >= 3) %>% 
  select(-Reference, -nrefs) %>% 
  as.matrix

rownames(matrixe) <- row_names_dfe

# Graphing where more than two citing paper ------------------------------------------------------------------

matrixe <- dfe %>%
  filter(nrefs >= 3) %>% 
  select(-Reference, -nrefs, -colour) %>% 
  as.matrix

rownames(matrixe) <- row_names_dfe

imatrix3a <- graph.incidence(matrixe, mode = "all")  # igraph matrix
# imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3a)
E(imatrix3a)


V(imatrix3a)$color[1:nrow(matrixe)] <- sphsu_cols("thistle") # University Blue
V(imatrix3a)$color[(nrow(matrixe)+1):(nrow(matrixe)+ncol(matrixe))] <- sphsu_cols("sunshine")  # Sunshine Yellow
V(imatrix3a)$size[1:nrow(matrixe)] <- 2
V(imatrix3a)$size[(nrow(matrixe)+1):(nrow(matrixe)+ncol(matrixe))] <- 12
V(imatrix3a)$label[1:nrow(matrixe)] <- NA
V(imatrix3a)$label[(nrow(matrixe)+1):(nrow(matrixe)+ncol(matrixe))] <- V(imatrix3a)$name[(nrow(matrixe)+1):(nrow(matrixe)+ncol(matrixe))]
V(imatrix3a)$frame.color <- NA
V(imatrix3a)$label.cex <- 0.3
E(imatrix3a)$color <- "#C0C0C080"

pdf('filtered.pdf')
plot(imatrix3a, layout = layout.fruchterman.reingold)
dev.off()

pdf('filtered2.pdf')
plot(imatrix3a, layout = layout.kamada.kawai)
dev.off()



# Messy AF graph ---------------------------------------------------------------------------------------------

row_names_df3b <- dfe %>% 
  pull(Reference)


matrix3b <- dfe %>%
  select(-Reference, -nrefs, -colour) %>% 
  as.matrix

rownames(matrix3b) <- row_names_df3b

imatrix3b <- graph.incidence(matrix3b, mode = "all")  # igraph matrix
# imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3b)
E(imatrix3b)

V(imatrix3b)$color[1:nrow(matrix3b)] <- sphsu_cols("leaf")  # University Blue
V(imatrix3b)$color[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- sphsu_cols("sunshine") # Sunshine Yellow
V(imatrix3b)$size[1:nrow(matrix3b)] <- 2
V(imatrix3b)$size[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- 12
V(imatrix3b)$label[1:nrow(matrix3b)] <- NA
V(imatrix3b)$label[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- V(imatrix3b)$name[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))]
V(imatrix3b)$frame.color <- NA
V(imatrix3b)$label.cex <- 0.3
E(imatrix3b)$color<-"#C0C0C080"

pdf('unfiltered.pdf')
plot(imatrix3b, layout = layout.fruchterman.reingold)
dev.off()

pdf('unfiltered2.pdf')
plot(imatrix3b, layout = layout.kamada.kawai)
dev.off()


# Graph with numbers -----------------------------------------------------------------------------------------

dff <- dfe %>% 
  filter(nrefs >= 3) %>% 
  mutate(id = paste0("[", 1:nrow(.), "]"))

row_names_df3f <- dff %>% 
  pull(id)


matrix3f <- dff %>%
  select(-Reference, -nrefs, -id, -colour) %>% 
  as.matrix

rownames(matrix3f) <- row_names_df3f

imatrix3f <- graph.incidence(matrix3f, mode = "all")  # igraph matrix
# imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3f)
E(imatrix3f)


V(imatrix3f)$color[1:nrow(matrix3f)] <- "white"
V(imatrix3f)$color[(nrow(matrix3f)+1):(nrow(matrix3f)+ncol(matrix3f))] <- "#FFDC36FF"  # Sunshine Yellow
V(imatrix3f)$size[1:nrow(matrix3f)] <- 4
V(imatrix3f)$size[(nrow(matrix3f)+1):(nrow(matrix3f)+ncol(matrix3f))] <- 12
V(imatrix3f)$frame.color <- NA
V(imatrix3f)$label.cex <- 0.3
E(imatrix3b)$color<-"#C0C0C080"

pdf('filteredposh.pdf')
plot(imatrix3f, layout = layout.fruchterman.reingold)
dev.off()

pdf('filteredposh2.pdf')
plot(imatrix3f, layout = layout.kamada.kawai)
dev.off()

write.csv(dff %>% select(id, Reference),"refs table filtered posh.csv" )

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

# Coloured reordered Messy ----------------------------------------------------------

dffr <- dfer %>% arrange(nrefs)

row_names_df3br <- dffr %>% 
  pull(Reference)


matrix3br <- dffr %>%
  select(-Reference, -nrefs) %>% 
  as.matrix

rownames(matrix3br) <- row_names_df3br

imatrix3br <- graph.incidence(matrix3br, mode = "all")  # igraph matrix
# imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3br)
E(imatrix3br)

V(imatrix3br)$color[1:nrow(matrix3br)] <- dfe$colour
V(imatrix3br)$color[(nrow(matrix3br)+1):(nrow(matrix3br)+ncol(matrix3br))] <- sphsu_cols("sunshine") # Sunshine Yellow
V(imatrix3br)$size[1:nrow(matrix3br)] <- 2
V(imatrix3br)$size[(nrow(matrix3br)+1):(nrow(matrix3br)+ncol(matrix3br))] <- 12
V(imatrix3br)$label[1:nrow(matrix3br)] <- NA
V(imatrix3br)$label[(nrow(matrix3br)+1):(nrow(matrix3br)+ncol(matrix3br))] <- V(imatrix3br)$name[(nrow(matrix3br)+1):(nrow(matrix3br)+ncol(matrix3br))]
V(imatrix3br)$frame.color <- NA
V(imatrix3br)$label.cex <- 0.3
E(imatrix3br)$color<-"#C0C0C080"

pdf('unfiltered.pdf')
plot(imatrix3br, layout = layout.fruchterman.reingold)
dev.off()

pdf('unfiltered2.pdf')
plot(imatrix3br, layout = layout.kamada.kawai)
dev.off()

pdf('unfiltered3.pdf')
plot(imatrix3br, layout = layout.gem)
dev.off()


# Coloured unfiltered -----------------------------------------------------



row_names_df3b <- dfe %>% 
  pull(Reference)


matrix3b <- dfe %>%
  select(-Reference, -nrefs, -colour) %>% 
  as.matrix

rownames(matrix3b) <- row_names_df3b

imatrix3b <- graph.incidence(matrix3b, mode = "all")  # igraph matrix
# imatrix3 <- delete.vertices(imatrix3, V(imatrix3)[degree(imatrix3)==0])
V(imatrix3b)
E(imatrix3b)

V(imatrix3b)$color[1:nrow(matrix3b)] <- dfe$colour
V(imatrix3b)$color[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- sphsu_cols("sunshine") # Sunshine Yellow
V(imatrix3b)$size[1:nrow(matrix3b)] <- 2
V(imatrix3b)$size[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- 12
V(imatrix3b)$label[1:nrow(matrix3b)] <- NA
V(imatrix3b)$label[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))] <- V(imatrix3b)$name[(nrow(matrix3b)+1):(nrow(matrix3b)+ncol(matrix3b))]
V(imatrix3b)$frame.color <- NA
V(imatrix3b)$label.cex <- 0.3
E(imatrix3b)$color<-"#C0C0C080"

pdf('unfiltered.pdf')
plot(imatrix3b, layout = layout.fruchterman.reingold)
dev.off()

pdf('unfiltered2.pdf')
plot(imatrix3b, layout = layout.kamada.kawai)
dev.off()
