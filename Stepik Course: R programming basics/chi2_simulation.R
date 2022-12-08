chi2 <- function(n,p,trials) {
  chi <- as.numeric()
  for (i in 1:trials) {
    t <- table(as.factor(rbinom(n, 1, prob = p)))
    chi[i] <- chisq.test(t)[1]
  }
  hist(as.vector(unlist(chi)), main = 'Histogram')
}

chi2(n=10000, p=0.5, trials = 10000)
chi2(n=60,p=0.5, trials = 1000)
hist(replicate(10000, sum(rnorm(9)^2)))

?replicate
?rnorm
?chisq.test
?rbinom

qchisq(0.95, 5)
?qchisq
chisq.test(c(10,10,10,5,10,15))$p.value
?chisq.test
rchisq(60, 5, ncp = 0)
attributes(chisq.test(c(10,30,50)))
chisq.test(c(10,30,50))$statistic
chisq.test(c(15,9))
chisq.test(c(795,705))


O <- matrix(c(18,6,7,13), ncol = 2)
chisq.test(O)$

#mosaic plots with conjugation table
b <- factor(c("Aspirin","Placebo"))
d <- factor(c("No","Yes"))
t = table(d, b, dnn = c('Tromboze', 'Group'))
t['No',]  = c(18,7)
t['Yes',] = c(6,13)
mosaicplot(t, main = '', color = T, shade = T)

#Fisher's exact test
K <- matrix(c(1,3,3,1), ncol = 2)
fisher.test(K)


#Homework Stepik Stats II

#Task 1, smart test
smart_test <-  function(x){
  for (i in x) {
    if (i < 5) {
      return(fisher.test(x)$p.value)
    } else {
      return(chisq.test(x)[['statistic']])
    }
  }
}
smart_test(matrix(c(6,5,9,9), ncol = 2))
