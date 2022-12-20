# Stepik Stats II: practice III
library(dplyr);library(tidyr);library(ggplot2);library(ape);library(FactoMineR);library(factoextra);library(ggrepel)
# Homework 1: add cluster_num column to a given df (Ncl is provided)  
# hclust(d, method = "complete", members = NULL)
# dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p =2)
df <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust<-  function(df, clnum){
  dist(df) %>% hclust() %>% cutree(3) %>% mutate(df, cluster = ., cluster = factor(cluster))
}

# Homework 2: after dividing your df in clusters, compare them with ANOVA and find the variables
# responsible for the difference that allows you to group your data in clusters
df <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
# names(df)[-length(names(df))] as an alternative for setdiff(names(df), "cluster")
get_difference <-  function(df, cln){
  df <- dist(df) %>% hclust() %>% cutree(cln) %>% mutate(df, cluster = ., cluster = factor(cluster))
  sapply(names(df)[-length(names(df))], 
        function(x) anova(aov(as.formula(paste(x, "~ cluster")),df))$P[1]) %>% {names(which(.<0.05))}
}

# Homework 3: Principal Component analysis
df <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc <- function(d) cbind(d, prcomp(d, rank. = 2)$x)
summary(a)
# Homework 4: after performing PCA, find out exactly how many PCs are enough to cover 90% of variance in given data
get_pca2 <- function(d){
  fit <- prcomp(d)
  rank <- 1 + max(which(summary(fit)$imp[3,] < 0.9))
  cbind(d, fit$x[, 1:rank])
}

# Homework 5: multicollinearity testing with PCA 
# Dplyr solution WIP
is_multicolDPLYR <- function(d){
  abs(prcomp(d, rank. = 1)$rotation) %>% 
  group_by(across(everything(.))) %>% 
    filter(n() == 2) 

  dff <- as_tibble(abs(prcomp(d, rank. = 1)$rotation, rownames_to_column()))
  group_by(across(everything()))
  tibble::has_rownames(dff)
}

# Hardcore base R solution
d <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
is_multicol <- function(d) {
  ds <- abs(signif(prcomp(d, rank. = 1)$rotation, 5))
  ldv <- ds[!(duplicated(ds) | duplicated(ds, fromLast = TRUE)), ]
  ldn <- dimnames(ds)[[1]][ds %in% ldv] # R deprecates dimnames in case of 1x1 matrix
  if (length(ldv) == length(d)) {"There is no collinearity in the data"}
  else {names(d)[!(names(d) %in% ldn)]}
}

# Homework 6: ggplotting
my_plot <- 
  dist(swiss) %>% hclust() %>% cutree(2) %>% mutate(swiss, cluster = ., cluster = factor(cluster)) %>% 
  ggplot(., aes(Education, Catholic, col = cluster))+
    geom_smooth(method = "lm")+
    geom_point()