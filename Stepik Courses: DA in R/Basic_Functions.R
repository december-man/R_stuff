# Functional programming
distr1 <- rnorm(1000)
hist(distr1)
distr1[1:30] <- NA
# Function to remove NA values
na_rm <- function(x) {
  if (is.numeric(x) == T) {
  stat_test <- shapiro.test(x)
    if (stat_test$p.value > 0.05) {
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values where substituted for a mean value")
    } else {
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values where substituted for a median value")
    }
  return(x)
  } else {
  print("X is not numeric")  
  }
}

# Function check
d1 <- rnorm(2000)
d2 <- runif(2000)
d1[1:10] <- NA
d2[1:10] <- NA

d1 <- na_rm(d1)  
d2 <- na_rm(d2)

# Loading outsourced function
d1 <- rnorm(1000)
d1[1:10] <- NA
source("NA_rm_func.R")
d1 <- na_rm(d1)

# Homework check
v <- c(1,3,2,4,5,NA,4,NA,NA)
is.na(v)
length(which(is.na(v)))

# Homework 1 Write a function that returns indices of NA values
indicesNA <- function(x) {
  return(which(is.na(x)))
}
# Homework 2 Write a function that counts NA values
countNA <- function(x) {
  return(length(which(is.na(x))))
}

# Binding multiple .csv frames together strictly while in a working space
csvrbind <- function() {
  df <- data.frame()
  number <<- 0
  for (i in dir(pattern = "*.csv")) {
    temp_df <- read.csv(i)
    df <- rbind(temp_df, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}
grantsAUTO <- csvrbind()
# <<- means global assignment, hence "number" variable will appear in a global R environment

# Homework 3 Write a function that sums only positive values in a given vector
positivesum <- function(x){
  sum(x[x > 0],na.rm = T)
}

#Homework 4 Write a function that finds and removes outliers in a data set
v <- c(rnorm(100), 3.2)
maxq <- IQR(v)*1.5
q <- quantile(v, probs = c(0.25, 0.75))
b1 <- q["75%"] + IQR(v)*1.5
b2 <- q["25%"] - IQR(v)*1.5

outliers.rm <- function(x){
  q <- quantile(x, probs = c(0.25, 0.75))
  x[!(x > q["75%"] + IQR(x)*1.5 | x < q["25%"] - IQR(x)*1.5)]
}
