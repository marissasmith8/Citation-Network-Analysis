library(tidyverse)
library(plotly)
library(igraph)
library(ggmosaic)
library(SPHSUgraphs)
library(patchwork)
source("./R/20 - Tidying dataframe to remove duplicates new.R")
source("C:/Users/marissa/Documents/Citation-Network-Analysis-New/Citation-Network-Analysis-New/R/biSBMR/biSBMWin.R")


# testing if it works  -------------------------------------------------------------
dfe_filtered <- filter(dfe, nrefs >1)
matrixe <- dfe_filtered %>%
    select(2:15) %>%
  as.matrix() %>% 
  `rownames<-`(dfe_filtered$Reference)

p <- graph.incidence(matrixe)

adj <- get.adjacency(p, sparse = FALSE)


# loop over multiple numbers of blocks ------------------------------------------------------

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
ka <- 1:15 #refs
kb <- 1:10 #guidelines
nodeType = c(rep(1, nrow(dfe_filtered)), rep(2, ncol(matrixe)))


third_test <- merge(ka, kb) %>% 
  as_tibble() %>% 
  filter(x>5|y>5) %>% 
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 5))) 
 

all_tests <- bind_rows(first_test, third_test) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))

points <- all_tests$score %>% 
  matrix(nrow = 15)

plot_ly(x = 1:10, y = 1:15, z = points) %>% add_surface()
api_create(last_plot(), "max_biSBM_score")

all_tests %>% 
  filter(y == 6) %>% 
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
  select(2:15) %>% 
  as.matrix() %>% 
  `rownames<-`(dfe_n2$Reference)

p2 <- graph.incidence(matrix2)

adj2 <- get.adjacency(p2, sparse = FALSE)

nodeType2 <-  c(rep(1, nrow(dfe_n2)), rep(2, ncol(matrix2)))

t3 <- biSBM(adj2, nodeType2, ka = 4, kb = 4, iter = 20)

t3$groups

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
  mutate(group_model = map2(x, y, ~biSBM(adj, nodeType, .x, .y, iter = 10))) %>% 
  mutate(score = map_dbl(group_model, ~ pluck(.x$score[1])))

save(points_repeat, all_tests, file = "data/biSBM_points.rda")

# starting from fresh session ---------------------------------------------

load("data/biSBM_points.rda")

points2 <- points_repeat$score %>% 
  matrix(nrow = 8, byrow = TRUE)


plot_ly(x = 1:8, y = 1:8, z = points2) %>% add_surface()
# api_create(last_plot(), "max_biSBM_score_filtered_10iter")
# Identified three points for testing

test_points <- tibble(x = c(1, 3, 4, 5, 6, 8),
                      y = c(1, 3, 3, 4, 6, 8))

test_points %>% 
  left_join(points_repeat, by = c("x", "y")) %>% 
  mutate(deg_freedom = x+y-2) %>% 
  arrange(deg_freedom) %>% 
  mutate(lr = -2*(lag(score, default = score[1]) - score),
         diffdeg = deg_freedom - lag(deg_freedom, default = 0),
         p = pchisq(lr, diffdeg, lower.tail = FALSE)) %>% 
  ggplot(aes(deg_freedom, score)) +
  geom_line()

merge(1:8,1:8) %>% 
  left_join(points_repeat, by = c("x", "y")) %>% 
  mutate(deg_freedom = x+y-2) %>% 
  group_by(deg_freedom) %>%
  summarise(score = max(score)) %>%
  left_join(points_repeat, by = c("score")) %>% 
  mutate(lr = -2*(lag(score, default = score[1]) - score),
         diffdeg = deg_freedom - lag(deg_freedom, default = 0),
         p = pchisq(lr, diffdeg, lower.tail = FALSE)) %>%
ggplot(aes(deg_freedom, score)) +
  geom_line()




# taking out models we want to compare ------------------------------------

key_points <- tibble(x = c(3, 4, 5, 6),
                     y = c(3, 4, 4, 6))

groupings <- points_repeat %>% 
  semi_join(key_points, by = c("x", "y")) %>% 
  mutate(model = map(group_model, ~ pluck(.x$groups))) %>% 
  select(x, y, model) %>% 
  unite("xy", x, y, sep = ":")

p1 <- dfe_filtered %>% 
  select(Reference) %>% 
  cbind(groupings$model[[1]][1:192]) %>% 
  cbind(groupings$model[[2]][1:192]) %>% 
  cbind(groupings$model[[3]][1:192]) %>% 
  cbind(groupings$model[[4]][1:192]) %>% 
  `names<-`(c("Reference", groupings$xy)) %>% 
  as_tibble() %>% 
  arrange(`3:3`, `4:4`, `5:4`, `6:6`) %>% 
  mutate_at(2:5, ~ as.numeric(factor(as.character(.x), levels = unique(.x)))) %>% 
  mutate(n = nrow(.):1,
         Reference = reorder(Reference, n)) %>% 
  pivot_longer(2:5, names_to = "n_groups", values_to = "group") %>% 
  mutate(n_groups = factor(n_groups)) %>% 
  ggplot(aes(n_groups, Reference, fill = factor(group))) +
  geom_tile() +
  scale_fill_sphsu() +
  theme_sphsu_light()


p2 <- dfe_filtered %>% 
  select(WHO.2014:APHA.2014) %>% 
  colnames() %>% 
  tibble() %>% 
  dplyr::transmute(GLdoc = str_replace(., "\\.", " ")) %>% 
  cbind(groupings$model[[1]][305:320]) %>% 
  cbind(groupings$model[[2]][305:320]) %>% 
  cbind(groupings$model[[3]][305:320]) %>% 
  cbind(groupings$model[[4]][305:320]) %>% 
  `names<-`(c("GLdoc", groupings$xy)) %>% 
  as_tibble() %>% 
  arrange(`4:4`, `5:4`, `5:5`, `5:6`) %>% 
  mutate_at(2:5, ~ LETTERS[as.numeric(factor(as.character(.x), levels = unique(.x)))]) %>% 
  mutate(n = nrow(.):1,
         GLdoc = reorder(GLdoc, n)) %>% 
  pivot_longer(2:5, names_to = "n_groups", values_to = "group") %>% 
  mutate(n_groups = factor(n_groups)) %>% 
  ggplot(aes(n_groups, GLdoc, fill = factor(group))) +
  geom_tile() +
  scale_fill_sphsu() +
  theme_sphsu_light()


p1+p2

# output tables ------------------------------------------

refs_grp <- dfe_n2 %>% 
  select(Reference) %>% 
  cbind(groupings$model[[1]][1:304]) %>% 
  cbind(groupings$model[[2]][1:304]) %>% 
  cbind(groupings$model[[3]][1:304]) %>% 
  cbind(groupings$model[[4]][1:304]) %>% 
  `names<-`(c("Reference", groupings$xy)) %>% 
  as_tibble() %>% 
  arrange(`4:4`, `5:4`, `5:5`, `5:6`) 

gls_grp <- dfe_n2 %>% 
  select(WHO.2014:APHA.2014) %>% 
  colnames() %>% 
  tibble() %>% 
  dplyr::transmute(GLdoc = str_replace(., "\\.", " ")) %>% 
  cbind(groupings$model[[1]][305:320]) %>% 
  cbind(groupings$model[[2]][305:320]) %>% 
  cbind(groupings$model[[3]][305:320]) %>% 
  cbind(groupings$model[[4]][305:320]) %>% 
  `names<-`(c("GLdoc", groupings$xy)) %>% 
  as_tibble() %>% 
  arrange(`4:4`, `5:4`, `5:5`, `5:6`)

# save(refs_grp, gls_grp, file = "data/biSBMgroups.rda")


# matrix plotting ---------------------------------------------------------
nodeType2 <-  c(rep(1, nrow(dfe_n2)), rep(2, ncol(matrix2)))

t3 <- biSBM(adj2, nodeType2, ka = 6, kb = 5, iter = 20)


sorted_refs <- dfe_n2 %>% 
  select(Reference, WHO.2014:APHA.2014) %>% 
  mutate(rf_group = t3$groups[t3$groups<=attr(t3, "a_grps")],
         # rf_group = case_when(
         #   rf_group == 1 ~ 1,
         #   rf_group == 2 ~ 4,
         #   rf_group == 3 ~ 2,
         #   rf_group == 4 ~ 3,
         #   TRUE ~ rf_group
         # ),
         Reference = reorder(Reference, rf_group)) 

gls_grp <- tibble(gls = colnames(matrix2),
                  gl_group = t3$groups[t3$groups>attr(t3, "a_grps")])  # %>%
# mutate(gl_group = case_when(
#   gl_group== 5 ~ 6,
#   gl_group== 6 ~ 5,
#   gl_group== 7 ~ 7,
#   gl_group== 8 ~ 8,
#   TRUE ~ gl_group
# ))

gls_breaks <-  gls_grp %>% 
  group_by(gl_group) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(n = cumsum(n)+ 0.5) %>% 
  pull(n) %>% 
  c(0,.)

rf_breaks <- sorted_refs %>% 
  group_by(rf_group) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(n = cumsum(n)+ 0.5) %>% 
  pull(n) %>% 
  c(0,.)


sorted_refs %>% 
  group_by(rf_group) %>% 
  pivot_longer(WHO.2014:APHA.2014,names_to = "gls", values_to = "cited") %>% 
  left_join(gls_grp, by = 'gls') %>% 
  arrange(rf_group, gl_group) %>% 
  mutate(gls = reorder(gls, gl_group)) %>% 
  ggplot(aes(Reference, gls, fill = factor(cited))) +
  geom_tile(colour = "white", size = 0.1) +
  scale_fill_manual(values = c('white', sphsu_cols("Thistle", names = FALSE))) +
  geom_hline(yintercept = gls_breaks, col = 'blue') +
  geom_vline(xintercept = rf_breaks, col = 'red') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.position = "none")



