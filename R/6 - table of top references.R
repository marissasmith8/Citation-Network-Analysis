# first output table for PECO checking ---------------------------------------------------------------------------

row_names4 <- paste0("[", 1:nrow(dfe_filtered), "]")

top_refs_table <- tibble(id = row_names4,
                         Reference = dfe_filtered$Reference,
                         `Number of times referenced in guidelines` = dfe_filtered$nrefs)

top_refs_table <- dfe_filtered %>% gather("Guidelines", "cited", -1) %>% 
  mutate(Guidelines = gsub("\\.", " ", .$Guidelines)) %>% 
  filter(cited == 1) %>% 
  select(-cited) %>% 
  group_by(Reference) %>% 
  summarise(Guidelines = paste0(Guidelines, collapse = ", ")) %>% 
  right_join(top_refs_table, by = "Reference") %>% 
  select(id, Reference, Guidelines, `Number of times referenced in guidelines`)

table_doc <- read_docx()

table_doc <- body_add_table(table_doc, top_refs_table)

print(table_doc, target = "Table of references by times cited.docx")


# tidy dataframe and join with new PECO table ----------------------------------------------------------------


