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

