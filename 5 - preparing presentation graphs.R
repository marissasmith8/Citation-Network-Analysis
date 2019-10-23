load("coords.rdata")

# import dataframe b and tidy  ---------------------------------------

dfe <- dfd %>% 
  mutate(nrefs = rowSums(.[2:17])) %>% 
  full_join(fill_dataframe_pres, by = "nrefs") %>% 
  filter(Reference != "No references given")

row_names_dfe <- dfe %>% 
  filter(nrefs >= 3) %>% 
  pull(Reference)


# ordered matrix ----------------------------------------------------------

dfe_ordered <- dfe %>% arrange(desc(nrefs))

row_names_dfe_ordered <- dfe_ordered %>% pull(Reference)

mt_allrefs_so <- dfe_ordered %>% 
  select(-Reference, -nrefs, -fill) %>% 
  as.matrix

row.names(mt_allrefs_so) <- row_names_dfe_ordered

reports <- colnames(dfe[,2:17])
reports
reports_formatted <- gsub("\\.(\\d{4}$)", "\n\\1", reports)
reports_formatted <- gsub("\\.", " ", reports_formatted)

colnames(mt_allrefs_so) <- reports_formatted

# unfiltered - new colours and shapes -------------------------------------



#  number of rows for refs/guidelines  
refs <- 1:nrow(dfe_ordered)
gls <- (nrow(dfe_ordered)+1):length(V(ig_allrefs_so))

#  guideline dots
V(ig_allrefs_so)$label[gls] <- reports_formatted
V(ig_allrefs_so)$color[gls] <- gl_cols$fill
V(ig_allrefs_so)$shape[gls] <- "square"
V(ig_allrefs_so)$size[gls] <- 12

#  ref dots
V(ig_allrefs_so)$label[refs] <- NA
V(ig_allrefs_so)$color[refs] <- dfe_ordered$fill
V(ig_allrefs_so)$shape[refs] <- "circle"
V(ig_allrefs_so)$size[refs] <- 2

#  all dots
V(ig_allrefs_so)$frame.color <- NA
V(ig_allrefs_so)$label.cex <- 0.4

#  edges
E(ig_allrefs_so)$color <- "#DDDDDD22"

#  interactive graph to get layout
tkplot(ig_allrefs_so, layout = layout.fruchterman.reingold, canvas.height = 768, canvas.width = 1366)

#  take coords from interactive plot
co_ig1 <- tk_coords("3")/400

svg("graphs/igraph - all refs - coloured - no groups_pres.svg")
plot(ig_allrefs_so, 
     # mark.groups = groups,  # group bubbles
     layout = co_ig1, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig1[,1])),
     ylim = c(0,max(co_ig1[,2])),
)
dev.off()

# filtered graph - col by nref --------------------------------------------

dfe_filtered <- dfe_ordered %>% filter(nrefs>2)

mt_fl_so <- dfe_filtered %>% 
  select(-Reference, -nrefs, -fill) %>% 
  as.matrix()

row.names(mt_fl_so) <- dfe_filtered$Reference
colnames(mt_fl_so) <- reports_formatted

# **igraph 3 - filtered and coloured -------------------------------------------------------------------------

ig_fl_so <- graph.incidence(mt_fl_so, mode = "all")

refs3 <- 1:nrow(dfe_filtered)
gls3 <- (nrow(dfe_filtered)+1):length(V(ig_fl_so))

# gl points
V(ig_fl_so)$label[gls3] <- reports_formatted
V(ig_fl_so)$color[gls3] <- gl_cols$fill
V(ig_fl_so)$shape[gls3] <- "square"
V(ig_fl_so)$size[gls3] <- 12

# ref points
V(ig_fl_so)$label[refs3] <- NA 
V(ig_fl_so)$color[refs3] <- dfe_filtered$fill
V(ig_fl_so)$shape[refs3] <- "circle"
V(ig_fl_so)$size[refs3] <- 4

# all points
V(ig_fl_so)$frame.color <- NA
V(ig_fl_so)$label.cex <- 0.35

#  edges
E(ig_fl_so)$color <- "#DDDDDD66"

tkplot(ig_fl_so, layout = layout.fruchterman.reingold, canvas.height = 768, canvas.width = 1366)

co_ig2 <- tk_coords("5")/450

svg("graphs/igraph - filtered - coloured - no groups.svg")
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = co_ig2, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(co_ig2[,1])),
     ylim = c(0,max(co_ig2[,2])),
)
dev.off()


# filtered coloured by conflicts ------------------------------------------



V(ig_fl_so)$color[refs3] <- 

  
  svg("graphs/igraph - filtered - col_conf.svg")
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = coords3, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(coords3[,1])),
     ylim = c(0,max(coords3[,2])),
)
dev.off()

# filtered coloured by study design ---------------------------------------

V(ig_fl_so)$color[refs3] <- 

  
  svg("graphs/igraph - filtered - col_stud.svg")
plot(ig_fl_so, 
     # mark.groups = groups,  # group bubbles
     layout = coords3, 
     mark.col = NA,   # group bubble fills
     rescale = FALSE,
     xlim = c(0,max(coords3[,1])),
     ylim = c(0,max(coords3[,2])),
)
dev.off()


# saving coords for plots -------------------------------------------------


save(co_ig1, co_ig2, file = "coords.rdata")
