df <- trial1[1:2887, ]

index <- which(is.na(df$Reference))

i <- 0
dfa <- list()

for (p in 1:17) {
  i <- i+1
  j <- index[p]-1
  dfa[[p]] <- data.frame(df[i:j, c(1, p+1)])
  i <- index[p]
}

dfb <- lapply(dfa, na.omit)

dfc <- tibble(Reference = "x")

q <- 1

for (q in 1:length(dfb)) {
  dfc <- dfc %>% 
    full_join(dfb[[q]], by = "Reference")
}

dfd <- dfc %>% 
  unique() %>% 
  filter(Reference != "x") %>% 
  replace(is.na(.), 0)