ggraph(ig_fl_so, layout = "kk") + 
  geom_edge_link() + 
  geom_node_point(aes(colour = color)) + 
  geom_node_label(aes(label = label)) +
  scale_colour_manual(labels = 1:8, values = fill_dataframe_pres$nr_fill)


layout <- create_layout(ig_fl_so, layout = "kk")

lo2 <- layout %>% 
  left_join(dfe_filtered %>% 
              select(Reference, nrefs, nr_fill) %>% 
              mutate(nrefs = as.character(nrefs)) %>% 
              bind_rows(gl_cols %>% transmute(Reference = Report, nrefs = as.character(context), nr_fill = fill)),
            by = c(name = "Reference", color = "nr_fill")) %>%
    mutate(x = co_ig2[,1], y = co_ig2[,2]) %>% 
  `attr<-`("graph", attr(layout, "graph")) %>% 
  `attr<-`("class", attr(layout, "class")) %>% 
  `attr<-`("circular", attr(layout, "circular"))




  ggraph(lo2) +
  geom_edge_link(colour = "grey") + 
  geom_node_point(aes(color = color, fill = color)) +
  geom_node_label(aes(fill = color,label = label), label.size = 0) +
    theme(rect = element_blank()) +
    scale_fill_identity(aesthetics = c("fill", "colour"))

tail(lo2, 16)

ggraph(layout) +
  geom_edge_link() + 
  geom_node_point(aes(color = color)) 

gl_cols
