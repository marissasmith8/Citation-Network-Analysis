# functions ---------------------------------------------------------------

concor <- function(matrix) {
  
  require(ggcorrplot)
  
  mt_out <- matrix
  t <- 1
  
  repeat {
    mt_out1 <- cor(mt_out)
    t <- t+1
    if (t==100){
      message("100 correlations? flip me!")
      break
    }
    if (identical(mt_out1, mt_out)) {
      break
    }
    
    mt_out <- mt_out1
    
  }
  
  message(paste("correlations completed", t, "times."))
  print(ggcorrplot(mt_out))
  return(mt_out)
  
}

split_matrix <- function(matrix, concor){
  
  grp1 <- concor[1,concor[1,]>0] %>% names()
  grp2 <- concor[1,concor[1,]<0] %>% names()
  
  mt1 <- matrix[,colnames(matrix) %in% grp1]
  mt2 <- matrix[,colnames(matrix) %in% grp2]
  
  return(list(mt1, mt2))
  
}



# ** first concor ---------------------------------------------------------

mt1 <- concor(mt)

new_mts <- split_matrix(mt, mt1)


# ** second concor --------------------------------------------------------

mt2 <- map(new_mts, concor)

new_mts2 <- map2(new_mts, mt2, split_matrix) %>% flatten()

# ** block matrix ---------------------------------------------------------

order <- map(new_mts2, colnames) %>% reduce(c)

mt_final <- mt[,order]

ggcorrplot(mt_final)

cor_mt <- cor(mt_final)

cor_mt[cor_mt < 0] <- 0
cor_mt[cor_mt > 0] <- 1

cor_mt %>% 
  ggcorrplot() + 
  geom_vline(size = 1.5, xintercept = 4.5) +
  geom_vline(size = 1.5, xintercept = 7.5) +
  geom_vline(size = 1.5, xintercept = 11.5) +
  geom_hline(size = 1.5, yintercept = 4.5) +
  geom_hline(size = 1.5, yintercept = 7.5) +
  geom_hline(size = 1.5, yintercept = 11.5)


# concor by docs ----------------------------------------------------------

mt1a <- concor(matrix2)

new_mtsa <- split_matrix(gls_by_doc, mt1a)

mt2a <- map(new_mtsa, concor)

new_mtsa2 <- map2(new_mtsa, mt2a, split_matrix) %>% flatten()

mt3a <- map(new_mtsa2, concor)

new_mtsa3 <- map2(new_mtsa2, mt3a, split_matrix) %>% flatten()

mt4a <- map(new_mtsa3, concor)

new_mtsa4 <- map2(new_mtsa3, mt4a, split_matrix) %>% flatten()


order <- map(new_mtsa4, colnames) %>% reduce(c)

group_splits <- map(new_mtsa4, ncol) %>% reduce(c) %>% cumsum() %>% head(., -1) +0.5

mt_final <- gls_by_doc[,order]

cor_mt <- cor(mt_final)


lines <- tibble(x = group_splits,
                y = group_splits)


lines <- tibble(u = group_splits,
                l = lag(group_splits, 1, 0),
                x = group_splits,
                y = group_splits) %>% 
  rowwise() %>% 
  mutate(laby = floor(mean(c(u, l))),
         labx = laby) %>% 
  ungroup() %>% 
  mutate(lab = 1:nrow(.))

cor_mt[cor_mt < 0] <- 0
cor_mt[cor_mt > 0] <- 1

cor_mt %>% 
  ggcorrplot() + 
  geom_hline(aes(yintercept = y), data = lines) +
  geom_vline(aes(xintercept = x), data = lines) +
  theme(axis.text = element_blank())

gls_by_doc %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE) +
  theme(axis.text = element_blank())


# combining levels (somehow?) ---------------------------------------------

matrix2 %*% mt1a %*% t(matrix2) %>% cor() %>% ggcorrplot() + ggcorrplot(mt1)


# converting to one-mode --------------------------------------------------

omm <- bipartite::as.one.mode(matrix2)

concor(omm)

