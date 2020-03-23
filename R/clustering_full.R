load("all_citations_matrix.rda")
library(bipartite)

start <- Sys.time()
modules_all <- computeModules(mt, method = "DormannStrauss", deep = TRUE)
end <- Sys.time()

print(end - start)

save(modules_all, file = "fullDS.rda")