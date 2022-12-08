# if & else
# if (condition) {do smth} else {do other thing}
# Commands in R are executed by rows
if (c(T,F) < 1.4) {
  print('bitch please')
} else { #thats why else is in between { {
  print('fucker please')
}
if (T) print('x')
#ifelse operator
?runif
ifelse(runif(8) > 0.5, 'Oryol', 'Reshka')
ifelse(runif(10) > 2/3, "'Kamen'",
       ifelse(runif(10) > 1/3, 'Nojnicy', 'Bumaga'))
#switch operator
switch("sum",
       sum = 5+5,
       product = 5*5,
       factorial = factorial(5),
       0)
#Cycles
#repeat
i <- 0
repeat {
  i <- i + runif(1)
  print(i)
  if (i > 10) break
}

#while
j <- 0.001
while (j < pi^7) {
  j <- j + sum(sample(c(0,2), 100, TRUE))
  print(j)
}
#for cycle
for (k in 1:10) {
  if (k %% 2 == 0) print(k)
}
for (x in letters) {
  if (x == 'a') next
  if (x == 'd') break
  print(x)
}
#for loop problem in R, ?system.time()
v <- 1:1e6
system.time({
  y <- 0
  for (i in v) y[i] <- sqrt(v[i])
})
#vs this:
system.time({
  z <- sqrt(v)
})
identical(y,z)
#Homeworks
#Number of values at a given interval in a randomized data set
set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
q <- 0
for (p in x) {
  if (p>-0.2 & p<0.3) q <- q + 1
}
#dice roll
dice_roll <- function(n) {
  return(sample(1:6, n, TRUE))
}
dice_roll(10)
