source("./R/1 - Tidying dataframe to remove duplicates.R")
library(tidyr)
library(stringr)
library(igraph)

# colour dataframe -------------------------------------------------------------------------------------------


ccols <- tibble(context =  as.factor(c("WHO",
                                       "UK",
                                       "AUS",
                                       "USA")),
                fill = c("#d3e585",
                         "#ffe771",
                         "#a6a6a6",
                         "#e3aa84"))



# guidline sizes ---------------------------------------------------------------------------------------------


dref_df <- dfe %>% 
  select(2:17) %>% 
  summarise_all(~ sum(.x)) %>% 
  gather("Guideline", "Nrefs") %>% 
  bind_rows(tibble(Guideline = c("NHS.2017", "ACS.2018"),
                   Nrefs = c(0, 0))) %>% 
  mutate(Guideline = str_replace_all(Guideline, "\\.", " ")) %>% 
  bind_cols(tibble(context = as.factor(c(rep("WHO", 2),
                                         rep("UK", 7),
                                         rep("AUS", 4),
                                         rep("USA", 3),
                                         "UK", "AUS")))) %>% 
  full_join(ccols, by = "context") %>% 
  # mutate(context = factor(context, ordered = TRUE)) %>% 
  group_by(context) %>% 
  mutate(n_gl = n()) %>% 
  ungroup() %>% 
  arrange(desc(n_gl), context, desc(Nrefs)) %>% 
  group_by(context) %>% 
  mutate(rank = 1:n_gl) %>% 
  ungroup() %>% 
  mutate(n_con = c(rep(1,8), rep(2, 5), rep(3, 3), rep(4, 2)),
         label = paste0(Guideline, "\n(", Nrefs, ")"))

mt <- diag(nrow = nrow(dref_df), ncol = nrow(dref_df))

row.names(mt) <- dref_df$Guideline
colnames(mt) <- dref_df$Guideline

# igraph -----------------------------------------------------------------------------------------------------

ig <- graph.adjacency(mt) 

V(ig)$size <- log10(dref_df$Nrefs+1)*10
V(ig)$label <- dref_df$label
V(ig)$frame.color <- NA
V(ig)$color <- dref_df$fill
V(ig)$label.cex <- 0.5

E(ig)$color <- NA

layout <-  as.matrix(tibble(a = (dref_df$rank-1)*16/50, b = (27 - (dref_df$n_con-1)*9)/30))
svg("./graphs/bubbles sized by nrefs.svg")
plot(ig,
     layout = layout, 
     xlim = c(-0.1,max(layout[,1])),
     ylim = c(0,max(layout[,2])),
     rescale = FALSE)
dev.off()
