# Apply function family. Stepik R in DA II
# apply(array, margin, ...)
# array - df, m, c()
# margin 1,2 - rows/columns (x,y,z...n)
# FUN - any func
apply(matrix(rnorm(30), nrow = 5), 2, sd)

# Homework 1 - find a median for all the columns with numeric vectors
library(ggplot2)
df <- diamonds
apply(df[sapply(df, is.numeric)], 2, median)

# min_max value in a column using range function
apply(matrix(rnorm(30), nrow = 5), 2, range)

# outliers
a <- apply(iris[, 1:4], 2, function(x) ifelse(length(x[abs(x - mean(x)) > 2*sd(x)]) > 0, "OUTLIERS!", "-"))

# Homework 2 Get negative values out of DF columns
test_data <- as.data.frame(list(V1 = c(-10.7, -10.2, NA, -9.6, -11.1, NA, NA), V2 = c(-10.1, -9.3, -12.1, -10.7, -8.5, NA, NA), V3 = c(-10.2, -11.1, NA, -9.1, -9.9, NA, NA), V4 = c(-10.4, -9.2, -9.9, -10, -10.1, NA, NA), V5 = c(8.8, 10.5, 10.6, NA, 10.4, NA, NA), V6 = c(-10.5, -10.3, NA, NA, -8.9, NA, NA), V7 = c(-11.1, -9.1, -10.2, -11.2, -8.9, NA, NA)))
get_negative_values <- function(test_data){
  test_data[is.na(test_data)] <- 0
  neg <- sapply(test_data, function(x) x[x<0])
  neg[sapply(neg, length) > 0]
}
get_negative_values(test_data)

# Homework 3 Substitute NA's with mean values
test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm  <- function(x){
  means <- function(x) { x[is.na(x)] <- mean(x, na.rm = T); x }
  as.data.frame(apply(x, 2, means))
}
na_rm(test_data)

# lapply - list as an input - list as an output, basically list$ are columns
# sapply - lapply with simplify = T, simplify transforms the output from list to an array/matrix

# Homework 4 positive sum func
lapply(d, function(d) sum(d[d>0], na.rm = T))

# tapply - same as aggregate() but aggregate returns df and can use formula input
tapply(mtcars$mpg, mtcars$am, mean)
aggregate(mpg ~ am, mtcars, mean)
# by - groups df by any variable
by(iris[, 1:4], iris$Species, colMeans)
by(iris[, 1:4], iris$Species, function(x) sapply(x, function(col) shapiro.test(col)$p.value))
# same but with aggregate
aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)

# vapply - same as lapply but with FUN.VALUE parameter to speed up calculations by depicting the desirable output 
# vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

# mapply - multivariate apply
# mapply(function, ...) - map(zip) in python
m <- matrix(rnorm(100 * 200), nrow = 100)
m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")
# paste("row"+sep+list[1][1], "row"+sep+list[1][n] end of list 1, list2 : "col"+sep+list[2][1]....)
# little remark about data types with df[] subsetting
#my_df[1] - dataframe
#my_df[[1]] - vector
#my_df[, 1] - vector

# Homework 5 united column frequency count
test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names <- c("HPS1", "HPS3")
library(magrittr); library(dplyr); library(tidyr)
my_names <- function (dataset, names){
  dataset %>% 
  separate(., col = name, into = c("name", "gene"), sep = "@") %>% 
  filter(., gene %in% names)
}
# Homework 6 find outliers in all possible groups of 1 numeric variable and n factors
df <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv", stringsAsFactors = T)
find_outliers <- function(df){
  num <- df[sapply(df, is.numeric)]; f <- df[!sapply(df, is.numeric)]
  means <- aggregate(num, f, mean); sds <- aggregate(num, f, sd)
  df <- merge(df, means, by = colnames(f)); df <- merge(df, sds, by = colnames(f))
  colnames(df) <- c(colnames(f), "x", "gmean", "gsd")
  df$is_outlier <- ifelse(abs(df$x - df$gmean) > 2*df$gsd, 1, 0); df
}
# Homework 7 Write a function that builds LM with predictors that passed normality test
smart_lm <- function(x){
  data <- x[apply(x, 2, function(x) shapiro.test(x)$p.value) > 0.05]
  if (ncol(data) == 0) {return("There are no normal variables in the data")}
  lm(data[[1]] ~ ., data[-1])$coefficients
}
# Homework 8 Write a function that runs one-sample t-test in a dataset with various variable types. Mu is given.
df <- iris
one_sample_t <- function(df, gm){
  lapply(df[sapply(df, is.numeric)], 
         function(df) 
           c(t.test(df, mu = gm)$statistic, t.test(df, mu = gm)$parameter, t.test(df, mu = gm)$p.value))
}
one_sample_t(iris[1:4], 4)
# Homework 9 Get p.value out of list of shapiro-wilk tests
get_p_value <- function(test_list){
  lapply(test_list, function(x) x$p.value)
}