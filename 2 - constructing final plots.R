library(dplyr)
library(readxl)
library(igraph)
library(SPHSUgraphs)
library(purrr)


# new report names -------------------------------------------------------------------------------------------

reports <- colnames(dfe[,2:17])
reports
reports_formatted <- gsub("\\.(\\d{4}$)", "\n\\1", reports)
reports_formatted <- gsub("\\.", " ", reports_formatted)

# creating groups --------------------------------------------------------------------------------------------


groups <- list(WHO = reports_formatted[c(1,2)],
               UK = reports_formatted[c(3:9)],
               AUS = reports_formatted[c(10:13)],
               USA = reports_formatted[c(14:16)]) 
groups_df <- groups %>% 
  map( ~ tibble(.x)) %>% 
  bind_rows(.id = "Country")

# One - mode matrix ------------------------------------------------------------------------------------------
omme <- t(matrixe) %*%  matrixe

igraph_omme <- graph.adjacency(omme, mode = "undirected")

E(igraph_omme)$weight <- count.multiple(igraph_omme)

igraph_omme <- igraph::simplify(igraph_omme)
# igraph_omme <- delete.vertices(igraph_omme, V(igraph_omme)[degree(igraph_omme)==0])

E(igraph_omme)$width <- log(E(igraph_omme)$weight)

plot(igraph_omme, mark.groups = groups, layout = layout.kamada.kawai)
plot(igraph_omme, mark.groups = groups, layout = layout.fruchterman.reingold)





# matrix of all points ---------------------------------------------------------------------------------------


mt_allrefs_un <- dfe %>% 
  select(-Reference, -nrefs, -fill) %>% 
  as.matrix

row_names_dfe <- dfe$Reference

row.names(mt_allrefs_un) <- row_names_dfe


#** igraph 1 - all_refs, no colour ----------------------------------------------------------------------------------------


ig_all_refs <- graph.incidence(mt_allrefs_un, mode = "all")
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
V(ig_all_refs)$label.cex <- 0.8

tkplot(ig_all_refs, layout = layout.fruchterman.reingold, canvas.height = 450, canvas.width = 800)

coords <- cbind(tk_coords("12")[,1]/300, tk_coords("12")[,2]/300)

svg('ig_all_refs.svg', width = 16, height = 9)
# pdf('ig_all_refs.pdf', width = 16, height = 9)
plot.igraph(ig_all_refs, layout = coords, rescale = FALSE,
            xlim = c(0,max(coords[,1])),
            ylim = c(0,max(coords[,2])),
            )

dev.off()


# ordered matrix ---------------------------------------------------------------------------------------------

dfe_ordered <- dfe %>% arrange(desc(nrefs))

row_names_dfe_ordered <- dfe_ordered %>% pull(Reference)

mt_allrefs_so <- dfe_ordered %>% 
  select(-Reference, -nrefs, -fill) %>% 
  as.matrix

row.names(mt_allrefs_so) <- row_names_dfe_ordered
colnames(mt_allrefs_so) <- reports_formatted


#** igraph 2 - ordered, coloured, non-filtered ----------------------------------------------------------------

ig_allrefs_so <- graph.incidence(mt_allrefs_so, mode = "all")

#  number of rows for refs/guidelines  
refs <- 1:nrow(dfe_ordered)
gls <- (nrow(dfe_ordered)+1):length(V(ig_allrefs_so))

#  guideline dots
V(ig_allrefs_so)$label[gls] <- reports_formatted
V(ig_allrefs_so)$color[gls] <- sphsu_cols("sunshine")
V(ig_allrefs_so)$size[gls] <- 12

#  ref dots
V(ig_allrefs_so)$label[refs] <- NA
V(ig_allrefs_so)$color[refs] <- dfe_ordered$fill
V(ig_allrefs_so)$size[refs] <- 2

#  all dots
V(ig_allrefs_so)$frame.color <- NA
V(ig_allrefs_so)$label.cex <- 0.3

#  edges
E(ig_allrefs_so)$color <- "#DDDDDD22"

#  interactive graph to get layout
tkplot(ig_allrefs_so, layout = layout.fruchterman.reingold, canvas.height = 450, canvas.width = 800)

#  take coords from interactive plot
coords <- tk_coords("3")/500

svg("graphs/igraph - all refs - coloured - no groups.svg")
plot(ig_allrefs_so, 
    # mark.groups = groups,  # group bubbles
     layout = coords, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(coords[,1])),
     ylim = c(0,max(coords[,2])),
)
dev.off()


# filtered df and matrix -------------------------------------------------------------------------------------


# **igraph 3 - filtered and coloured -------------------------------------------------------------------------

#** igraph 4 - filtered and numbered -------------------------------------------------------------------------
