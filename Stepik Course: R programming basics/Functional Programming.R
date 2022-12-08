# Object-Oriented systems

# S3 Class
# no formal declaration of a class
# generic functions
# method dispatch (varying behavior for the same function in different class)

# S4 Class
# Rigid definition of a class
# More possibilities with method dispatch

# Reference Classes -> Advanced R book

# Generic functions

methods(print)
length(methods(print))
# 187 methods based on a different task, varying with argument type
# print.default(x) is called if x doesnt match to any of the methods

# COPY-ON-MODIFY SEMANTICS
# on a CALL-BY-VALUE basis
# if a variable isn't passed in a local environment (function envir. for example), it will be searched in a global one.

# Replicate function/method

get_status <- function(n, p = 0.1) {
  x <- rbinom(n, 1, p)
  sum(x)
}

replicate(10, get_status(100))

# Mapply function - Multi-dimensional version of apply
mapply(seq, from = 1:4, to = 2:5, by = 1/(1+1:4))
#Same as:
list(
  seq(1,2, 1/2), seq(2,3, 1/3),
  seq(3,4, 1/4), seq(4,5, 1/5)
)

# outer function (combinations of elements in vectors)
m <- outer(letters, LETTERS, paste0)
dim(m)
diag(m)
m[1:5,1:5]

# Vectorize function - Vectorizing non-vectorized functions
lp_norm <- function(x, p = 2) {
  if (p >= 1) sum(abs(x)^p)^(1/p) else NaN
}
lp_norm(1:10, -1:5)
# Warning! condition with length > 1
# Vectorize!
lp_norm <- Vectorize(lp_norm, "p")
lp_norm(1:10, -1:4)

# do.call - function call on a list of arguments - extremely useful
df1 <- data.frame(id = 1:2, value = rnorm(2))
df2 <- data.frame(id = 3:4, value = runif(2))
df3 <- data.frame(id = 222, value = 7)
dftot <- do.call(rbind, list(df1, df2, df3))

# Generalized version of the rbind code
do.call(rbind, lapply(list.files(), function(file) read.csv(file)))

# Homework 1 - Cat catalogue
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")
catcat <- sort(outer(outer(outer(cat_temper, cat_color, paste), cat_age, paste), cat_trait, paste))

# Homework 2 - methods of generic functions
funs <- c("print","summary","plot")
meths <- lapply(funs, methods)
grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)

# Homework 3 - local/global variable/ copy-on-modify fuckery
f <- function(y) {
  y <- x + y
  y
}

g <- function(x) {
  y <- f(x)
  f <- function(x) {
    y - x
  }
  y - f(x)
}

x <- 10
y <- 1
f(x); f(y)
g(x); g(y)
x; 

# Time and elegance
m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) 
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}

m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}
  
m3 <- function(x, y) x %o% y

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

library(microbenchmark)
microbenchmark(m1(x, y), m2(x, y), m3(x, y))

# Homework 4 Random walk with absorption
# Random walk with absorption
simulate_walk <- function(x = 0, y = 0, r = 6, n_max = 100, p = 1e-2) {
  current_position <- c(x, y)
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(1)
    current_position <- current_position + c(rnorm(1),rnorm(1))
    if (sqrt(sum(current_position^2)) >  r) return(2)
  }
  return(3)
}
# Simulate results
result <- replicate(10000, simulate_walk(), simplify = TRUE)
# Probability count
length(result[result == 2])/100000
