library(tidyverse)
library(plotly)
library(igraph)
source("./R/1 - Tidying dataframe to remove duplicates.R")
source("R/biSBMR/biSBMWin.R")


# unfiltered --------------------------------------------------------------

matrixe <- dfe %>% 
  select(WHO.2014:APHA.2014) %>% 
  as.matrix() %>% 
  `rownames<-`(dfe$Reference)

p <- graph.incidence(matrixe)

adj <- get.adjacency(p, sparse = FALSE)

t1 <- biSBM(adj, nodeType = c(rep(1, nrow(dfe)), rep(2, ncol(matrixe))), ka = 1, kb = 2)


# t2 <- biSBM(adj, nodeType = c(rep(1, nrow(dfe)), rep(2, ncol(matrixe))), ka = 4, kb = 4)
t2
adj

# running function manually -----------------------------------------------
# 
# ka <- 1
# kb <- 2
# data <- adj
# nodeType <- c(rep(1, nrow(dfe)), rep(2, ncol(matrixe)))
# deg.corr <- 1
# iter <- 1
# 
# 
# if(any(c(ka<0, kb<0, deg.corr<0, iter<0))) stop("ka, kb, deg.corr and iter must be positive");
# 
# ### deg.corr can be just 0 or 1
# if(deg.corr>1 || deg.corr<0) stop("Use deg.corr=1 for the degree corrected SBM and 0 otherwise");
# 
# ### loading the shared library
# dyn.load("R/biSBMR/biSBMWin_R.dll");
# if(!is.loaded("rFunction")) stop("Something is wrong with the C++ shared library");
# 
# ### if the adjacency matrix is supplied, transform it in edgelist
# ###
# if(typeof(data)=="list") data <- vapply(data, as.double, numeric(nrow(data)));
# if(ncol(data)>2){
#   nn <- ncol(data); ##total nodes
#   data[lower.tri(data)] <- 0;
#   nodesA <- which(rowSums(data)==0)[1] - 1;
#   el1 <- rep(1:nodesA, rowSums(data[1:nodesA, ]));
#   el2 <- unlist(apply(data[1:nodesA,], 1, function(x) which(x!=0)));
#   data <- cbind(el1, el2);
# }
# 
# if(min(data)==0){
#   warning("Network indexing starts from 0", call.=F, immediate.=T);
#   network <- network + 1;
#   
# } 
# 
# ## checking dimensions and type for nodeType
# if(typeof(nodeType)=="list") nodeType <- vapply(nodeType, as.double, numeric(nrow(nodeType)));
# if(is.atomic(nodeType)) dim(nodeType) <- c(length(nodeType), 1);
# if((sum(dim(nodeType)) - abs(diff(dim(nodeType))))!=2) stop("nodeType is not a column or row vector");
# if(!is.double(nodeType)) nodeType <- as.double(nodeType);
# 
# if(length(nodeType)!= length(unique(as.numeric(data)))) stop("The number of network's nodes and node types are different. Please, 
#           check the connected components in your network");
# 
# 
# ### calling C++ algorithm and returning the partition
# res <-integer(length(nodeType));
# score <-  numeric(1)
# t <- .C("rFunction", as.numeric(data), nrow(data), nodeType, length(nodeType), as.integer(ka), 
#    as.integer(kb), as.integer(deg.corr), as.integer(iter), res=res, score = score)
# 


# loop over multiple ------------------------------------------------------

ka <- 1:5
kb <- 1:5
nodeType = c(rep(1, nrow(dfe)), rep(2, ncol(matrixe)))

first_test <- merge(ka, kb) %>% 
  as_tibble() %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 5)))

points <- first_test %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1]))) %>% 
  select(-group_model)


matrix(points$score,nrow = 5)

library(plotly)

plot_ly(x = 1:5, y = 1:5, z = matrix(points$score,nrow = 5)) %>% 
  add_surface()


# new test - increase only refs # -----------------------------------------

second_test <- tibble(x = 1:10, y = 5) %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 1))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))

second_test %>% 
  ggplot(aes(x, score)) + geom_line()



# long_test ---------------------------------------------------------------
ka <- 1:15
kb <- 1:10

third_test <- merge(ka, kb) %>% 
  as_tibble() %>% 
    filter(x>5|y>5) %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 5))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))

all_tests <- bind_rows(first_test, third_test)

points <- all_tests$score %>% 
  matrix(nrow = 15)

plot_ly(x = 1:10, y = 1:15, z = points) %>% add_surface()
api_create(last_plot(), "max_biSBM_score")

all_tests %>% 
  filter(y == 5) %>% 
  ggplot(aes(x, score)) + 
  geom_line() +
  xlab("y")

all_tests %>% 
  ggplot(aes(x, score)) +
  geom_line() +
  facet_wrap(~y) +
  geom_vline(xintercept = 5, color = "red")

# filtered dataset --------------------------------------------------------

dfe_n2 <- dfe %>% 
  filter(nrefs > 1) 

matrix2 <- dfe_n2 %>% 
  select(WHO.2014:APHA.2014) %>% 
  as.matrix() %>% 
  `rownames<-`(dfe_n2$Reference)

p2 <- graph.incidence(matrix2)

adj2 <- get.adjacency(p2, sparse = FALSE)

nodeType2 <-  c(rep(1, nrow(dfe_n2)), rep(2, ncol(matrix2)))

t3 <- biSBM(adj2, nodeType2, ka = 2, kb = 1, iter = 1)

t3

ka <- 1:15  # relabelled x
kb <- 1:10  # relabelled y

filtered_test <- merge(ka, kb) %>% 
  as_tibble() %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj2, nodeType2, .x, .y, iter = 5))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))


points <- filtered_test$score %>% 
  matrix(nrow = 10, byrow = TRUE)

plot_ly(x = 1:15, y = 1:10, z = points) %>% add_surface()
api_create(last_plot(), "max_biSBM_score_filtered")



# more iterations ---------------------------------------------------------

ka <- 1:8
kb <- 1:8

points_repeat <- merge(ka, kb) %>% 
  as_tibble() %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj2, nodeType2, .x, .y, iter = 10))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))


points2 <- points_repeat$score %>% 
  matrix(nrow = 8, byrow = TRUE)
  

plot_ly(x = 1:8, y = 1:8, z = points2) %>% add_surface()
api_create(last_plot(), "max_biSBM_score_filtered_10iter")
# Identified three points for testing

test_points <- tibble(x = c(4, 5, 5, 6, 7),
                      y = c(4, 3, 6, 5, 3))

ll_input <- test_points %>% 
  left_join(points_repeat, by = c("x", "y")) %>% 
  mutate(deg_freedom = x*y-1) %>% 
  arrange(deg_freedom)

-2*(ll_input$score[1] - ll_input$score[2])

# save(points_repeat, file = "data/biSBM_points.rda")
