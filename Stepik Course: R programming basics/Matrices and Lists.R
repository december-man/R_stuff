x <- matrix(1:100, nrow = 9, byrow = T)
?matrix
is.matrix(x)
y <- rep(1:10,2)
yy <- as.matrix(y)
# Единственное отличие матрицы от вектора, это наличие атрибута dim - размерности.
attributes(x)
attributes(yy)
# Почему-то у за матрицу не считается. Видимо as.matrix не создаёт новой переменной у, в которой она представлена в виде матрицы.
dim(x)
c(nrow(x),ncol(x))
dim(yy) <- NULL
dim(yy) <- c(4,5)
#Arithmetic
a <- matrix(1:4, nrow = 2, ncol = 2)
b <- matrix(c(1,2,2,3), nrow = 2, ncol = 2)
#Поэлементные операции:
a+b
a*b
a/b
a^b
a-b
a %% b
a + 7
7*(a+b)
# С умножением подвох, есть матричное умножение, есть поэлементное умножение
a %*% b
det(a %*% b)
# Индексирование
a[2,2]
a[,2]
a[1,]
a[1,] <- c(0,0)
a[,2] <- 2:3
a[2,] <- 0
a[-1,] <- 1
a[-2,] <- 3
a[-2,-1]
# dimension wrap
ind <- c(2,1)
d <- matrix(1:10, ncol = 5)
d[,ind]
d[,ind, drop = T]
attributes(d[,ind, drop = F])
# Rownames and Colnames
rownames(d) <- c("row1","row2")
attributes(d)
colnames(d) <- paste0("column", 1:5)
d["row1", c("column1","column4"), drop = F]
#Присоединение матриц, rbind cbind
rbind(a,b)
cbind(a,b)
# Ellipsis argument '...' means you can pass any number of objects to a function
rbind(a,b,c(1,21,4,52,534,63,34),1:10,rbind(a,b,rbind(a,b)))

# Apply function

m <- matrix(1:25,5)
f <- function(x) sum(x^2)
apply(m,1:2,f)
apply(m, 1:2, f <- function(i) if (i>13) i else 12)
#or use anonymous function
m[m <= 13] <- 13; m
# rowSums, rowMeans, colSums, colMeans
rowSums(m)
colMeans(m)
apply(m, 1, sum)
apply(m, 2, mean)
all.equal(rowSums(m), apply(m, 1, sum))
all.equal(colMeans(m), apply(m, 2, mean))
# Homework
n <- matrix(1:16, nrow = 4, ncol = 3)
n[4,3]
n[, 3, drop = F]
attributes(n[4,])
attributes(n[4, , drop = F])
n > 5
# Homework 2
g <- function(v,n) {
  dif <- abs(v-n)
  return(which(dif == min(dif)))
}
# Bind matrices diagonally
bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill, 
               nrow = nrow(m1)+nrow(m2), 
               ncol = ncol(m1)+ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1)+1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
  m3
}
m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)
bind_diag(m2, m1, fill = 0)
# Homework 3 build a ziccurat
ziccuratt <- function(n) {
  z <- matrix(NA, nrow = 2*n - 1 , ncol = 2*n - 1)
  for (i in 1:n) {
    z[i:(2*n-i), i:(2*n-i)] <- matrix(i, nrow = length(i:(2*n-i)) , ncol = length(i:(2*n-i)))
    print(z)
  }
  return(z)
}
ziccuratt(4)


# LISTS

# Элементы списка могут быть разного типа, в отличие от вектора
list(1:5, "mydata", matrix(sqrt(2),2,2))
list(a=1,b=3,"1to5" = 1:5)
#recursion in lists
list(b+a, list(a,list(a=42)))
l1 <- list(char = "fuf", "sus", 42)
l2 <- list(chacha = "fuffa", "syys", 43)
c(l1,l2)
v <- 1:7
j <- list(v) #LIST OF 1 vector !
#unlist
l <- list(3:4,4:2, last = 6)
unlist(l)
unlist(c(l,"spy")) # vector of strings! Auto-conversion
#indexing
#returns a sub-list!!
l[1]
l["last"]
#access by name $, partial commentary works too! return a single lement
l$last
l$l
#access to a single element
l[[1]]
l[[1]] <- NULL
l[[4]] <- 99 
l # [[3]] is now NULL, because it got deleted and we pushed index to 4
l <- list(vec = 1:7, func = sqrt)
l$func(4) #super cool, accessing function directly from a list
names(l)
is.null(l$string) #check non-existent elements by name
l$string <- "Citrus" #adding element by name
l[[2]](4) # B L O W N
names(l)

# Apply with lists list-apply

l <- list(a = c("12","34"), b = LETTERS[5:10], c = 1:5)
lapply(l, length)
lapply(l, paste, collapse = "?")
# simplified apply (list to vector)
sapply(l, paste, collapse = "|") #cool!
# using $ to minimize list element's name
l <- list(some_name = 0, incredibly_goddamn_long_bloody_name = 1)
l$inc + 1 # aaaay! #partial matching

#Homework 4 the DIAG func

diag(1)
class(diag(1))
diag(c(1,2))
diag(matrix(pi,2,2))
attributes(diag(matrix(pi,2,2)))

#Homework 5 the table of contents of vector
# using table and/or unique
x <- c(5, 2, 7, 7, 7, 2, 0, 0)
M <- matrix(NA, nrow = 2, ncol = length(sort(unique(x))))
M[1,] <- sort(unique(x))
M[2,] <- as.vector(table(x))
typeof(M)
class(M)
#using only table
class(as.matrix(table(x)))
H <- as.matrix(table(x))
#using aggregate and rotating matrix (transposing)
?aggregate
M <- data.matrix(t(aggregate(x, by = list(num = x), length))) # magic list(num=x)
# Bastille question
set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2), 
  "Tresor Tower" = rbinom(8, 12, 1/4), 
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5), 
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1), 
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
  )
bastille
Ntot <- (lapply(bastille, sum))
sum(as.numeric(Ntot))
