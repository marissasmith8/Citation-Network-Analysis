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
?scale_size_manual


df_country_cols <- tibble(x=1:4, 
                          y=1:4,
                          Country = factor(c("WHO", "UK", "Australia", "USA"))
                            )

col = c(
  "#ff0000",
  "#7fff00",
  "#00ffff",
  "#7f00ff")

names(col) <- df_country_cols$Country

ggplot(df_country_cols, aes(x, y, col = Country)) +
  geom_line(size = 2) +
  theme_void() +
  scale_colour_manual(values = col)
  # scale_colour_manual(values = (df_country_cols$Country = df_country_cols$col))

export::graph2ppt(last_plot(),"group_legend.pptx")


# nrefs for numbered graph -----------------------------------------------------------------------------------



dfe_filtered %>% 
  select(nrefs) %>%
  group_by(nrefs) %>% 
  add_tally() %>% 
  unique() %>% 
  ggplot(aes(factor(nrefs), n, fill = factor(nrefs))) + geom_histogram(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = col_matrix2) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  ylab("Number of references") +
  xlab("Number of times cited across Guidelines")


export::graph2ppt(last_plot(), "legend - count of refs by times cited.pptx", width = 1, height= 3)



# table of top refs ------------------------------------------------------------------------------------------

row_names4 <- paste0("[", 1:nrow(dfe_filtered), "]")

top_refs_table <- tibble(id = row_names4,
                         Reference = dfe_filtered$Reference,
                         `Number of times referenced in guidelines` = dfe_filtered$nrefs)

table_doc <- read_docx()

table_doc <- body_add_table(table_doc, top_refs_table)

print(table_doc, target = "Table of references by times cited.docx")
