source("./R/20- Tidying dataframe to remove duplicates new.R")
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


dref_df <-  dfe %>% 
  select(2:14) %>% 
  summarise_all(~ sum(.x)) %>% 
  gather("Guideline", "Nrefs") %>% 
  bind_rows(tibble(Guideline = c("NHS.2017"),
                   Nrefs = c(0, 0))) %>% 
  mutate(Guideline = str_replace_all(Guideline, "\\.", " ")) %>% 
  bind_cols(tibble(context = as.factor(c(rep("WHO", 2),
                                         rep("UK", 6),
                                         rep("AUS", 2),
                                         rep("USA", 3),
                                         "UK", "USA")))) %>% 
  full_join(ccols, by = "context") %>% 
  # mutate(context = factor(context, ordered = TRUE)) %>% 
  group_by(context) %>% 
  mutate(n_gl = n()) %>% 
  ungroup() %>%
  arrange(desc(n_gl), context, desc(Nrefs)) %>% 
  group_by(context) %>% 
  mutate(rank = 1:n_gl) %>% 
  ungroup() %>% 
  mutate(n_con = c(rep(1,8), rep(2, 4), rep(3, 4), rep(4, 2)),
         label = paste0(Guideline, "\n(", Nrefs, ")"))

mt <- diag(nrow = nrow(dref_df), ncol = nrow(dref_df))

row.names(mt) <- dref_df$Guideline
colnames(mt) <- dref_df$Guideline


# ggplot ------------------------------------------------------------------

cols <- ccols$fill
names(cols) <- ccols$context

dref_df %>% ggplot(aes(x = context, y = rank, col = context, size = Nrefs)) +
  geom_point(shape = 16) + 
  coord_flip() + 
  theme(legend.position = "none",
        rect = element_blank(),
        axis.ticks = element_blank()) +
  scale_y_continuous(expand = c(0.11, 0), "Rank by number of references", breaks = NULL) +
  scale_x_discrete("Context", limits = rev(c("UK", "AUS", "USA", "WHO"))) +
  scale_size_continuous(range = c(1,30)) +
  scale_color_manual(values = cols) +
  geom_text(aes(label = label, size = NULL), size = 3, col = "#000000")

export::graph2ppt(last_plot(), "graphs/size by nrefs_paper.pptx", height = 4, width = 7)

  # igraph -----------------------------------------------------------------------------------------------------

ig <- graph.adjacency(mt) 

# V(ig)$size <- (dref_df$Nrefs+1)/50
V(ig)$size <- log10(dref_df$Nrefs+1)*8
V(ig)$label <- dref_df$label
V(ig)$frame.color <- NA
V(ig)$color <- dref_df$fill
V(ig)$label.cex <- 0.7

E(ig)$color <- NA

layout <-  as.matrix(tibble(a = (dref_df$rank-1)/4, b = (3 - (dref_df$n_con-1))/4))
svg("./graphs/bubbles sized by nrefs.svg")
plot(ig,
     layout = layout, 
     xlim = c(-0.1,max(layout[,1])),
     ylim = c(0,max(layout[,2])),
     rescale = FALSE)
dev.off()
