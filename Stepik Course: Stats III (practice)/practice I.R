# Homework 1 - heteroscedasticity checker - build auxiliary LR and yield its Rsq
hetero_test <- function(df) {
  e <- lm(df[,1] ~ ., df[-1])$residuals
  summary(lm(e^2 ~ ., df[-1]))$r.squared
}
# Homework 2 - build VIF calculator
VIF <- function(d) {
  vif <- sapply(1:ncol(d[-1]), function(x) 1 / (1 - summary(lm(d[-1][, x] ~., d[-1][-x]))$r.sq))
  names(vif) <- names(d[-1]); vif
}
# Homework 3 - LM optimization by vif sorting - remove predictors with VIF > 10 one by one until none left
smart_model <-  function(d){
  vif <- VIF(d)
  if (length(vif[vif > 10]) == 0) return(lm(d[, 1] ~ ., d[-1])$coef)
  d <- d[-(which.max(vif[vif>10])+1)]
  if (ncol(d) == 2) return(lm(d[, 1] ~ ., d[-1])$coef)
  return(smart_model(d))
}
# Homework 4 - Find optimal lambda for Tukey's power transformation
# lambda range is already given
transform_x <- function(d){
  lr <- seq(-2, 2, 0.1)
  rsq <- sapply(lr, function(l) summary(lm(y ~ I(x^l), d))$r.sq)
  rsq[which(lr == 0)] <- summary(lm(y ~ log(x), d))$r.sq
  l <- lr[which.max(rsq)]
  ifelse(l == 0, d$x <- log(d$x), ifelse(l < 0, d$x <- -(d$x)^l, d$x <- (d$x)^l))
  ggplot(d, aes(log(x),y)) + geom_point() + geom_smooth(method = "lm")
}


