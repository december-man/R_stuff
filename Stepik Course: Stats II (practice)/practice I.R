# Stepik: Statistics II, R practice
# Write a function that the hypothesis of independency of two given variables using chi2 criteria or fisher's exact test
x <- as.data.frame(list(am = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), vs = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1)))
smart_test <-  function(x){
  x <- table(x)
  if (all((x) > 5)) {
    res <- chisq.test(x)
    return(c(res$statistic, res$parameter, res$p.value))
  } else {
    res <- fisher.test(x)
    return(res$p.value)
  }
}
smart_test(x)

# Write a function which gets DF with n cols as an argument, where every column is a nucleotide sequence and returns
# a vector with colnames that passed chi2 test with p.value <0.05 (H0: nucleotides distributed evenly)
library(dplyr)
x <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = T)
most_significant <-  function(x) {
  counters <- NULL
  pvalues <- NULL
  for (i in 1:length(x)) {
    counters[[i]] <- count(x, x[[i]])
    pvalues[[i]] <- chisq.test(counters[[i]]["n"])$p.value
  }
  return(colnames(x[which(pvalues == min(as.numeric(pvalues)))]))
}
# more elegant through table and sapply
most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
  min_p  <- which(chisq_tests == min(chisq_tests))    
  return(colnames(test_data)[min_p])
}
# Create a new categorial variable in dataset iris, judging by 4 main numerical parameters and their mean values,
# whether this row of data represents an "important case" or not
df <- iris
colMeans(iris[1:4])
comp <- t(apply(iris[1:4], 1, function(x) x >= colMeans(iris[1:4])))
df$important_cases <- factor(ifelse(rowSums(ifelse(comp, 1, 0)) >= 3, 1, 0), labels = c("No", "Yes"))
table(df$important_cases)
# more elegant solution
iris$important_cases <- as.factor(ifelse(colSums(t(iris[,1:4]) > colMeans(iris[,1:4])) >= 3, "Yes", "No"))

# Create a function for the previous task but for any number of columns
get_important_cases <- function(x){
  m <- colMeans(x); t = floor(ncol(x)/2)
  x$important_cases <- factor(apply(x, 1, function(x) ifelse(sum(x > m) > t, "Yes", "No"))); x
}
df <- mtcars[,c(1,3,4,5,6,7)]
get_important_cases(test_data)

# Create a function that returns the mode of a given vector
stat_mode <- function(x){
  dimnames(table(x))$x[which(as.vector(table(x)) == max(as.vector(table(x))))]
}
# more elegant solution
stat_mode <- function(x) {
  as.numeric(names(which(table(x) == max(table(x)))))
}
# Find the dimnames of value of standartized residues in a contingency table
df <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv", stringsAsFactors = T)
max_resid <- function(x){
  res <- chisq.test(table(x))$stdres
  loc <- which(res == max(res), arr.ind = T)
  return(c(rownames(res)[loc[1]], colnames(res)[loc[2]]))
}
# ggplotting
library(ggplot2)
ggplot(diamonds, aes(x = color, fill = cut))+
  geom_bar(position = "dodge")
