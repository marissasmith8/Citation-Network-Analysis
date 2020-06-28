=library(tidyr)
library(stringr)

# table by conflicts of interest ------------------------------------------

gls <- factor(c(gsub("\\.", " ", colnames(full_dfe[4:19])), "Total"), ordered = TRUE)


table2 <-
  full_dfe %>% 
  filter(nrefs > 2) %>% 
  select(3:19) %>% 
  group_by(conf) %>% 
  summarise_all( ~ sum(.)) %>% 
  ungroup() %>% #mutate(Total = rowSums(.[2:17])) %>% 
  gather("Guidelines", "n", -1, factor_key = TRUE) %>% 
  spread(conf, n)%>% 
  mutate(Total = rowSums(.[3:9]))

table2[table2$Guidelines == 'Total', 'Total'] <-  97

table3 <- table2 %>% select(-Context) %>% 
  mutate(Guidelines = factor(gsub("\\.", " ", .$Guidelines), ordered = TRUE)) %>% 
  gather("Conflicts of interest", "n", -c(1, 9))  %>% 
  mutate(n = paste0(n, " (", round(n*100/Total, 1), "%)")) %>% 
  spread(`Conflicts of interest`, n) %>% 
  gather("Conflicts of interest", "n", -1)  %>% 
  spread(Guidelines, n) %>% 
  select(1, as.character(gls))

totals <- as.numeric(as.vector(table3[table3$`Conflicts of interest`=='Total',2:17]))

table3[table3$`Conflicts of interest`=='Total',2:17] <- paste0(totals," (", round(totals*100/97, 1), "%)")

write.csv(table3, "outputs/Conflicts table 1.csv", row.names = FALSE)

# # table4 <-
#   table2 %>% select(-Guidelines) %>% 
#     group_by(Context) %>% 
#     summarise_all(~ sum(.)) %>%
#     mutate(Context = ifelse(is.na(Context), 'Total', as.character(Context))) %>% 
#   gather("Conflicts of interest", "n", -c(1, 9))  %>% 
#   mutate(n = paste0(n, " (", round(n*100/Total, 1), "%)")) %>% 
#   spread(`Conflicts of interest`, n) %>% 
#   gather("Conflicts of interest", "n", -1)  %>% 
#   spread(Context, n) %>% 
#   select(1, as.character(gls))

table4 <- full_dfe %>% 
  filter(nrefs > 2) %>% 
  select(3:19) %>% 
  mutate(id = 1:nrow(.)) %>% 
  # mutate(conf = paste0(1:nrow(.), conf)) %>% 
  gather("Guidelines", "n", -c(1, 18)) %>% 
  full_join(tibble(Guidelines = colnames(full_dfe[4:19]), Context = gl_cols$context)) %>% 
  select(-Guidelines) %>% 
  unique() %>% 
  select(-id) %>% 
  group_by(Context, conf) %>% 
  summarize(n = sum(n)) %>% 
  spread(Context, n) %>% 
  bind_rows(., summarise_at(., 2:5, ~sum(.))) %>% 
  mutate(conf = ifelse(is.na(.$conf), "Total", .$conf)) %>% 
  left_join(cn_labels %>% ungroup() %>% select(conf, Total= n) %>% bind_rows(tibble(conf = "Total", Total = 97))) %>% 
  gather("con", n, -1) %>% 
  spread(conf, n) %>% 
  gather("Conflicts of interest", "n", -c(1, 9))  %>% 
  mutate(n = paste0(n, " (", round(n*100/Total, 1), "%)")) %>% 
  spread(`Conflicts of interest`, n) %>% 
  gather("Conflicts of interest", "n", -1)  %>% 
  spread(con, n) %>% 
  select(c(1, 6, 4, 2, 5, 3))

totals2 <- as.numeric(as.vector(table4[table4$`Conflicts of interest`=='Total',2:5]))

table4[table4$`Conflicts of interest`=='Total',2:5] <- paste0(totals2," (", round(totals2*100/97, 1), "%)")

write.csv(table4, "outputs/Conflicts table 2.csv", row.names = FALSE)
