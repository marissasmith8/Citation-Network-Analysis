legend_df <- tibble(number = 1:8,
                    y = 1)


bar_fill <- sphsu_cols("leaf",
           "turquoise",
           "Rose",
           "university blue",
           "Rust",
           "Thistle", 
           "Moss",
           "pumpkin")

names(bar_fill) <- 1:8

ggplot(legend_df, aes(x = factor(number), fill = factor(number))) + 
  geom_bar() +
  scale_fill_manual(values = bar_fill) +
  labs(fill = "Number of times cited\nacross guidelines:")

fill_dataframe <- tibble(nrefs = 1:8,
                   fill = bar_fill)

sphsu_palettes$matrix <- sphsu_cols("leaf", "turquoise","thistle", "burgundy",  "university blue", "cobalt", "rust", "pumpkin")

ggplot(legend_df, aes(x = factor(number), fill = factor(number))) + 
  geom_bar() +
  scale_fill_sphsu("matrix") +
  labs(fill = "Number of times cited\nacross guidelines:")
