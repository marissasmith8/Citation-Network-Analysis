library(dplyr)
library(readxl)
library(igraph)
library(SPHSUgraphs)
library(visNetwork)


# new report names -------------------------------------------------------------------------------------------

reports <- colnames(dfe[,2:17])
reports
reports_formatted <- gsub("\\.(\\d{4}$)", "\n\\1", reports)
reports_formatted <- gsub("\\.", " ", reports_formatted)

# creating groups --------------------------------------------------------------------------------------------

row_names_dfe

groups <- list(reports[c(1,2)], reports[c(3:9)], reports[c(10:13)], reports[c(14:16)])

# One - mode matrix ------------------------------------------------------------------------------------------

matrixe <- dfe %>% select(-Reference, -nrefs, -fill) %>% 
  as.matrix

row_names_dfe <- dfe$Reference

row.names(matrixe) <- row_names_dfe

omme <- t(matrixe) %*%  matrixe

igraph_omme <- graph.adjacency(omme, mode = "undirected")

E(igraph_omme)$weight <- count.multiple(igraph_omme)

igraph_omme <- igraph::simplify(igraph_omme)
# igraph_omme <- delete.vertices(igraph_omme, V(igraph_omme)[degree(igraph_omme)==0])

E(igraph_omme)$width <- log(E(igraph_omme)$weight)

plot(igraph_omme, mark.groups = groups, layout = layout.kamada.kawai)
plot(igraph_omme, mark.groups = groups, layout = layout.fruchterman.reingold)




# visnetwork - not working -------------------------------------------------------------------------------------------------

# nodes_e_reports <- tibble(id  = reports, type = "report", country = c(1,1,2,2,2,2,2,2,2,3,3,3,3,4,4,4))
# nodes_e_papers <- tibble(id = row_names_dfe, type = "paper", country = NA)
# 
# igraph_tm_full <- graph.incidence(matrixe, mode = "all")
# 
# visIgraph(igraph_tm_full)
# 
# nodes_e <- bind_rows(nodes_e_reports, nodes_e_papers)
# 
# visNetwork(data.frame(id = V(igraph_omme)), E(igraph_omme))



# igraph 1 - all_refs ----------------------------------------------------------------------------------------

ig_all_refs <- graph.incidence(matrixe, mode = "all")
ig_all_refs <- delete.vertices(ig_all_refs, V(ig_all_refs)[degree(ig_all_refs)==0])

E(ig_all_refs)


E(ig_all_refs)$color <- "#DDDDDD"
V(ig_all_refs)$label[1:2421] <- NA
V(ig_all_refs)$label[2422:2437] <- reports_formatted
V(ig_all_refs)$size[1:2420] <- 2
V(ig_all_refs)$size[2422:2437] <- 12
V(ig_all_refs)$color[1:2421] <- "#00a64d"
V(ig_all_refs)$color[2422:2437] <- sphsu_cols("sunshine")
# V(ig_all_refs)$label[1:2421] <- 
# V(ig_all_refs)$label[2422:2436] <- 
V(ig_all_refs)$frame.color <- NA
V(ig_all_refs)$label.cex <- 0.4

pdf('ig_all_refs.pdf')
svg('ig_all_refs2.svg')
# plot(ig_all_refs, layout = layout.kamada.kawai)
plot(ig_all_refs, layout = layout.fruchterman.reingold, width = 1920, height = 1080)
dev.off()
