# Stats III practice III. Bootstrap

# Homework 1 Bootstrapping CI for sample median
median_cl_boot <- function(x){
  bootstrap_med <- sort(replicate(1000, median(sample(x, size = length(x)/2 , replace = T)) - median(x)))
  return(bootstrap_med[c(50,950)] + median(x))
}

# Homework 2 Bootstrapping CI for LM slope coefficient
library(dplyr)
df <- as_tibble(x = matrix(rnorm(50, 5), rnorm(50, 2), nrow = 50, ncol = 2))
dff <- mtcars
slope_cl_boot <- function(df){
  ssl <- lm(mpg ~ hp, dff)$coef[[2]]
  bootstrap_med <- sort(replicate(1000, lm(mpg ~ hp, dff[sample(nrow(dff), replace = T),])$coef[[2]] - ssl))
  return(bootstrap_med[c(50,950)] + ssl)
}
