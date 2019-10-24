library(tidyverse)
library(ggplot2)
library(SPHSUgraphs)

source("./R/1 - Tidying dataframe to remove duplicates.R")


# creating dataframes ----------------------------------------------------------------------------------------

legend_df <- tibble(x = factor(1:8),
                    y = 1)  # dummy dataframe for all dots



# Nrefs legend (for filtered and unfiltered) ------------------------------------------------------------

gr_labels <- dfe_ordered %>% group_by(nrefs) %>% 
  count() %>% 
  mutate(labels = paste0(nrefs, " (", n, ")"))

ggplot(legend_df, aes(x, y, col = factor(number))) + 
  geom_point() +
  scale_color_manual(values = fill_dataframe_pres$fill, labels = gr_labels$labels) +
  labs(col = "Number of times\ncited across\nguidelines (n):") +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_void()

export::graph2ppt(last_plot(), "Presentation colour legend.pptx")



# study design legend ----------------------------------------------------------------------------------------

st_labels <- dfe_filtered %>% 
  select(stud, st_n, st_fill) %>% 
  unique()

ggplot(legend_df, aes(x, y, col = as.numeric(x))) %>% 
  


# paper colour scheme [to be split up] -----------------------------------------------------

sphsu_palettes$matrix <- sphsu_cols("leaf", "turquoise","thistle", "burgundy",  "university blue", "cobalt", "rust", "pumpkin")

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



ggplot(df_colours, aes(x, y, col = factor(number), size = group)) +
  geom_point() +
  scale_colour_manual(name = "References by\nnumber of times\ncited across\nGuidelines:", values = col_matrix2) +
  scale_size_manual(name = "", values = c("Guideline" = 15, "ref" = 5)) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size=5)))

export::graph2ppt(last_plot(),"scale.pptx")





# guideline context colours legend -----------------------------------------------
gl_cols %>% 
  ggplot(aes(Report, fill = context)) + 
  geom_bar() +
  theme_void() +
  scale_fill_manual("Context", values = ccols$fill, labels = ccols$context)

export::graph2ppt(last_plot(), "Context legend.pptx")