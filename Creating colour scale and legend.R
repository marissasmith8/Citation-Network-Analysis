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



df_colours <- tibble(
  group = c("Guideline", rep("ref", 8)),
  number = 0:8,
  y = 1:9,
  x = 1:9
)

sphsu_palettes$matrix2 <- sphsu_cols("sunshine", "leaf", "turquoise","thistle", "burgundy",  "university blue", "cobalt", "rust", "pumpkin")

ggplot(df_colours, aes(x, y, col = factor(number), size = group)) +
  geom_point() +
  scale_colour_sphsu(name = "References by\nnumber of times\ncited across\nGuidelines:", "matrix2") +
  scale_size_manual(name = "", values = c("Guideline" = 15, "ref" = 5)) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size=5)))

export::graph2ppt(last_plot(),"scale.pptx")
?scale_size_manual


df_country_cols <- tibble(x=1:8, 
                          y=1:8,
                          Country = rep(factor(c("WHO", "UK", "Australia", "USA")), 2)
                            )

col = c(
  "#ff0000",
  "#7fff00",
  "#00ffff",
  "#7f00ff")

names(col) <- df_country_cols$Country[1:4]

ggplot(df_country_cols, aes(x, y, col = Country)) +
  geom_line(size = 2) +
  theme_void() +
  scale_colour_manual(values = col)
  # scale_colour_manual(values = (df_country_cols$Country = df_country_cols$col))

export::graph2ppt(last_plot(),"group_legend.pptx")
