library(tidyverse)
library(ggplot2)
library(SPHSUgraphs)
library(ggpubr)

source("./R/1 - Tidying dataframe to remove duplicates.R")


# creating dataframes ----------------------------------------------------------------------------------------

legend_df <- tibble(x = factor(1:8),
                    y = 1)  # dummy dataframe for all dots

# Nrefs legend (for filtered and unfiltered) ------------------------------------------------------------

gr_labels <- dfe_ordered %>% group_by(nrefs) %>% 
  count() %>% 
  mutate(labels = paste0(nrefs, " (", n, ")"))

p <- ggplot(legend_df, aes(x, y, col = x)) + 
  geom_point() +
  scale_color_manual("Number of times\ncited across\nguidelines (n):",
                     values = fill_dataframe_paper$nr_fill, labels = gr_labels$labels) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_void()

# svglite::svglite("outputs/legent_nrefs_paper.svg")
as_ggplot(get_legend(p))
# dev.off()

export::graph2ppt(last_plot(), "outputs/legend_nrefs_paper.pptx")



# study design legend ----------------------------------------------------------------------------------------

st_labels <- full_dfe %>% 
  select(stud, st_n, st_fill) %>% 
  group_by(stud, st_n, st_fill) %>% 
  count() %>% 
  unique() %>% arrange(desc(n)) %>% 
  mutate(labels = paste0(stud, " (", n, ")"))



p <- ggplot(legend_df, aes(x, y, col = x)) +
  geom_point() +
  scale_colour_manual("Type of citation:", values = st_labels$st_fill, labels = st_labels$labels) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_void() # +
  # theme(legend.background = element_rect(fill = "#003865"),
        # text = element_text(color = "white"))

as_ggplot(get_legend(p))

export::graph2ppt(last_plot(), "outputs/legend_stud_paper.pptx")

# Conflicts legend -------------------------------------------------------------------------------------------

cn_labels <- full_dfe %>% 
  select(conf, cn_n, cn_fill) %>% 
  group_by(conf, cn_n, cn_fill) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(label = paste0(conf, " (", n, ")")) %>% 
  unique()

p <- ggplot(legend_df %>% filter(as.numeric(x)<8), aes(x, y, col = x)) +
  geom_point() +
  scale_colour_manual("Conflicts of interest:", values = cn_labels$cn_fill, labels = cn_labels$label) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_void()# +
  # theme(legend.background = element_rect(fill = "#003865"),
  #       text = element_text(color = "white"))

as_ggplot(get_legend(p))

export::graph2ppt(last_plot(), "outputs/legend_conf_paper.pptx")


# paper colour scheme [to be split up] -----------------------------------------------------

# paper_fill <- sphsu_cols("leaf", "turquoise","thistle", "burgundy",  "university blue", "cobalt", "rust", "pumpkin", names = FALSE)

ggplot(legend_df, aes(x = factor(number), fill = factor(number))) + 
  geom_bar() +
  scale_fill_sphsu("matrix") +
  labs(fill = "Number of times cited\nacross guidelines:")



df_colours <- tibble(
  group = c("Guideline", rep("ref", 8)),
  number = 0:8,
  y = 1:9,
  x = 1:9
)

col_matrix2 <- sphsu_cols("sunshine","leaf",
                          "turquoise",
                          "Rose",
                          "university blue",
                          "Rust",
                          "Thistle", 
                          "Moss",
                          "pumpkin")

names(col_matrix2) <- 0:8



p <- ggplot(df_colours, aes(x, y, col = factor(number), size = group)) +
  geom_point() +
  scale_colour_manual(name = "References by\nnumber of times\ncited across\nGuidelines:", values = col_matrix2) +
  scale_size_manual(name = "", values = c("Guideline" = 15, "ref" = 5)) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size=5)))

as_ggplot(get_legend(p))

export::graph2ppt(last_plot(),"outputs/scale.pptx")





# guideline context colours legend -----------------------------------------------
p <- gl_cols %>% 
  ggplot(aes(Report, fill = context)) + 
  geom_bar() +
  theme_void() +
  scale_fill_manual("Context", values = ccols$fill, labels = ccols$context)

as_ggplot(get_legend(p))

export::graph2ppt(last_plot(), "outputs/legend_context.pptx")