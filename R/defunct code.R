
# parts not currently used -----------------------------------------------------------------------------------

# histogram of times cited [not used] -----------------------------------------------------------



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



# context legend (for all graphs) ------------------------------------------------------------



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