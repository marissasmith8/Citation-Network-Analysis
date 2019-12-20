# importing and reading all guidelines ------------------------------------

paths <- paste0("./Documents/Public health recommedations/Documents/", list.files("./Documents/Public health recommedations/Documents", recursive = TRUE))

library(pdftools)
library(purrr)
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(SPHSUgraphs)

all_gls <- map(paths, ~ pdf_text(.x))

tidied_all <- all_gls %>% map(~ str_split(.x, "\r\n") %>%
                                reduce(c) %>% 
                                str_trim() %>% 
                                str_extract(".+") %>% 
                                unique() %>% 
                                tibble(text = .) %>% 
                                filter(!is.na(text)))

reference_index <- tidied_all %>% map(~ .x$text %>% grep("^references", ., ignore.case = TRUE))  

names <- paths %>% 
  gsub("(^.*Recommendations/)(.*)(\\.pdf$)", "\\2", ., ignore.case = TRUE) %>% 
  str_remove_all("PDF Files/")

names2 <- paths %>% 
  gsub("(^.*Documents/)(\\w+)(/.*$)", "\\2", ., ignore.case = TRUE) %>% 
  paste0(., " #", c(1:4, 1:8, 1:3, 1,2))

names(tidied_all) <- names

for (i in 1:length(names)){
  tidied_all[[i]] <- tidied_all[[i]] %>%  mutate(document = names2[[i]])
}

all_df <- reduce(tidied_all, bind_rows)

all_df %>%
  unnest_tokens(word, text) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(document, word, sort = TRUE) %>% 
  bind_tf_idf(word, document, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = reorder_within(word, tf_idf, document)) %>%
  group_by(document) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = document)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ document, scales = "free") +
  coord_flip() +
  theme_sphsu_light() +
  scale_fill_sphsu() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank()) +
  scale_x_reordered()
