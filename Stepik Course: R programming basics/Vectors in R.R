# Batch regime
# R script document
hello <- 'hello world'
print(hello)
#basics
c(1,3,2)
c(3,2,1)
0.1+0.1 == 0.2
0.1+0.05 == 0.15 #double var type problem, it's not 0.15 exactly
#comparator all.equal
all.equal(0.1+0.05,0.15)
# 1 Task: thirds and sevenths
a <- seq(0, 1, 1/3)
b <- seq(0, 1, 1/7)
d <- sort(unique(c(a,b)))
# Homework
get_fractions <- function(m,n) {
  e <- seq(0,1, 1/m)
  f <- seq(0,1, 1/n)
  return(sort(unique(c(e,f)), decreasing = TRUE))
}
get_fractions(100,300)
#vector types
typeof(c(pi:pi^2))
is.double(c(pi*cos(pi^2):pi^5))
is.logical(pi)
is.logical(0)
is.logical(1)
is.logical(T)
is.logical(t)
is.logical(F)
b <- c(T, FALSE, 1.5, 5, pi, sqrt(-1), 'abc')
typeof(b)
#transforming vectors in certain data types
as.character(b)
#length
x <- 100:-1
length(x)
length(x) <- 4
x
length(x) <- 5
x
#named vectors
a <- c(uno = 1, dos = 2, tres = 3, 'universal answer' = 42)
names(a) <- c('one', 'two', 'three', 'forty two')
a
#Vectorization. Arithmetic operators are used 'by element'!
2:5 + c(1,1,1,1)
2:5 * c(2,2,2,2)
c(T,T,T) + 0:2
c(T,T,T) & 0:2
sqrt(1:10)
seq(0,10, length = 4)
floor(seq(0,10, length = 4))
#Not vectorized:
sum(1:100)
#Valid names in R, restrictions & examples
.hidden <- 1
super_long_name_in_fact_so_long_i_cannot_stop_typing_please_help <- 1
next <- 1
:smiley: <- 1
ls(all.names = TRUE)

# Vectors pt.2 
#Recycling (operations with vectors of different legnths)
0:5 + 0:1
0:5*0:1
1:5 + 0:2
1:5 * 0:2
#same as:
1:5 * c(0,1,2,0,1)
#scalar on vector
3 + 3:10
3 ^ 3:10
(3:10) ^ 3
#Using logical operators:
2:5 == 3
2:5 <= 4
# Access to vector values:
x <- seq(10,100, by = 10)
val <- x[1]
x[]
x
x[3:7]
x[c(1,2,3,4,5,6,2,3,4,5:6,3:2,2:9,x[1])]
x[x[x[1]]]
x[x[1]]
#negative indices skipping the n-th element
x[-1]
x[-(1:9)]
x[-(length(x))]
x[-sqrt(x[1]+6)]
x[-c(1,3,4,5,6)]
#Logical indices
x[rep(c(TRUE,FALSE),5)]
x[c(TRUE,FALSE)] #recycling rule works here, prolonging c vector to length of x vector
x[c(TRUE,TRUE,FALSE,TRUE,TRUE)]
x[x > 1 & x < 50] #awesome!
#named vectors
a <- c(1,2,42)
names(a) <- c('one','two','forty two')
a[c('one', 'two')]
a['one']
# ALL and ANY functions
all(x<=100)
all(x<200)
any(x>5)
any(x>100)
# Which function
which(x >= 50)
which(x >=100)
which(x >=101)
which.max(x)
which.min(x)
# Attributes and properties
#length() is a property, it always exists
#an attribute like names() can be given, it's not there by default
x <- c(5,3,9)
names(x) <- c('V','III','IX')
attr(x, 'author' ) <- 'Caesar'
attributes(x)
attr(x, 'bullshit market') <- 'TRUE'
attributes(x) <- NULL
attributes(x)
#Homework
?'%%'
5 %% 2
5 %/% 2
4 %% 2
z <- seq(0,30, by = 1)
z[-(1:7)]
#WATCH OUT FOR THE OPERATOR PRIORITY! #1
rep(T,6)
c(rep(T,6),F)
z[c(rep(T,6),F)]
z[-(seq(7,length(z),by=7))]
z[1:length(z) %% 7 !=0]
z[1:length(z) %% 7 > 0]
z[-(1:floor(length(z)/7) * 7)]
#2 max, which, which.max
max(5)
which.max(5)
which(5)
which(c(T,F))
max(1:10)
which(1:10)
which.max(1:10)
which(5 >= 5)
n <- seq(0,10, by = 1)
which (n < 1)
which (n < 2)
#returns indices of appropriate elements (vector output)
which(n == T)
which.min(n)
#returns indices of minimal/maximal elements (scalar output)
which.max(1:2) #any input except for literal string
which.max("asdmjakrgmrktg")
max(c("1", "99", "HI"))
which.max(c("1", "99", "HI"))
# which - это функция, которая применяется для логического типа и возвращает индексы TRUE
# max - выдаст наибольший элемент. Для чисел - в математическом смысле, для строк - в лексиграфическом
# which.max - выдаст номер наибольшего числа в векторе. 
# При этом число может быть записано как цифрой, так и строковым элементом. 
# Но строковые элементы типа "TRUE" or "1*2" конвертированы в число не будут. Только если просто число в кавычках, грубо говоря

#Part 2. Fizz-buzz, for cycle style
y <- vector(mode ='character', length = 100)
y <- character(100) #same as line 103
for (i in 1:100) {
  if (i %% 15 == 0) {
    y[i] <- "fizz-buzz"
  } else if (i %% 3 == 0) {
    y[i] <- "fizz"
  } else if (i %% 5 == 0) {
    y[i] <- "buzz"
  } else {
    y[i] <- i
  }
}
y
# Fizz-buzz, vectorization style (R-style)
x <- 1:100
z <- 1:100
x %% 5 == 0
z[x %% 5 == 0] <- 'buzz' #vector magic!
z[x %% 3 == 0] <- 'fizz'
z[x %% 15 == 0] <- 'fizz-buzz'
all(y==z)
#Homework
s <- letters                     # вектор букв
s[c(1, 23, 5, 19, 15, 13, 5)]    # индексы вектора \
#Geometric Progression
x <- 2 ^ (0:10)
log2(x)
#Some randomness
set.seed(42)
x <- sample(1:100,50)
#Neighbors with greatest diff 
x[-1] # -1 and last values cannot be neighbors
x[-length(x)]
x[-1] - x[-length(x)] #substract two vectors from each other
k <- which.max(abs(x[-1] - x[-length(x)])) #take absolute value, get index
k
x[c(k,k+1)]
x
#Multiple min/max
x <- sample (1:100,50, replace = TRUE)
min(x)
which.min(x) # 'REPLACE' vulnerability (multiple mins/maxs but it returns the first occurence index)
which(x == min(x))
#lets make a function
maxdiff <- function(x) {
  y <- abs(x[-1] - x[-length(x)])
  k <- which(y == max(y))
  p <- c(x[k],x[k+1])
  print("Neighbor pair:")
  print(p)
  print("Maxdiff is:")
  print(max(y))
}
xx <- sample(1:100, 1e4, replace= TRUE)
maxdiff(xx)
##Homework
z <- sample(1:20, 50, replace= TRUE)
diff(z)
is_monotone <- function(v) {
  y <- diff(v)
  if (all(y >= 0)) {
    return(TRUE)
  } else if (all(y <= 0)) {
    return(TRUE)
  } else {
    return(FALSE) 
  }
}
j <- c(0,0,0,0,1,2,3,3,4,5,5,5,5,7)
is_monotone(j)
#Shorter version
is_monotone_shorter <- function(v) {
  all(diff(v) >= 0) | all(diff(v) <= 0)
}
is_monotone_shorter(j1)
#Homework 3
k <- seq(1,50, by = 1)
k[T]
k['one']
k[c(T,T,F)]
k[length(k)]
k[1:3]
#Homework 4
combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions == FALSE) {
    return((factorial(n))/(factorial(k)*factorial(n-k)))
  } else if (with_repretitions == TRUE) {
    return((factorial(n+k-1))/(factorial(k)*factorial(n-1)))
  }
}
combin_count(4,3,TRUE)