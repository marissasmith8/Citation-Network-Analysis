# creating graph of citation numbers by doc/context -----------------------


count_refs <- dfb %>% map_df(~ mutate(.x, doc = colnames(.x)[2]) %>% 
                 group_by(doc) %>% 
                 count()) %>% 
  bind_cols(tibble(context = as.factor(c(rep("WHO", 2),
                               rep("UK", 8),
                               rep("AUS", 4),
                               rep("USA", 4))))) %>% 
  ungroup() %>% 
  mutate(doc = gsub("\\.(\\d{4}$)", "\n\\1", .$doc)) %>% 
  mutate(doc = gsub("\\.", " ", .$doc))

#graphing

count_refs %>% ggplot(aes(context, fill = context)) +
  geom_dotplot(aes(size = n), method = "histodot", binwidth = 1, col = NA) +
  coord_flip()

count_refs %>% ggplot(aes(context, fill = context)) +
  geom_bar(aes(y = n, fill = doc), stat = "identity", position = "stack") +
  coord_flip() +
  theme(legend.position = "none") + 
  geom_text(aes(y = n, label = doc), position = "stack")
