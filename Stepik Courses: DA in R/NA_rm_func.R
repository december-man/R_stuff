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