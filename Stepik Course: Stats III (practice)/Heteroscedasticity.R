library(ggplot2)
# Tukey's power ladder and Box-Cox tf with MASS library
library(MASS)
df <- diamonds
fit <- lm(price ~ carat, df)
summary(fit)
lambda <- boxcox(price ~ carat, data = df, plotit = T, eps = 1/50, xlab = "lambda value", ylab = "log-likelihood")
# extracting the exact lambda
lambda <- lambda$x[which.max(lambda$y)]
# Box-Coxing the predictor (could just use any value from Tukey's power ladder for testing)
df$carat_exact <- ((df$carat ^ lambda) - 1) / lambda
# before
ggplot(df, aes(x = price, y = carat))+
  geom_point(size = 0.05)+
  geom_smooth(method = "lm")
# after
ggplot(df, aes(x = log(price), y = carat_exact))+
  geom_point(size = 0.05)+
  geom_smooth(method = "lm")
# heteroscedasticity tests with skedastic
library(skedastic)
# White's test
white(lm(price ~ carat, diamonds), interactions = F, statonly = T) # whoops. 21.7 Gb vector size wtf...
pairs(swiss)
wt <- white(lm(Agriculture ~ Infant.Mortality, swiss), interactions = F) # anything above p_0.05 is homoscedastic
# Breusch - Pagan / Koenker test
bp <- breusch_pagan(fit, koenker = F) # full heteroscedasticity
koenker <- breusch_pagan(fit, koenker = T) # same
# Let's apply log tf
fitlog <- lm(log(price) ~ log(carat), diamonds)
summary(fitlog) # better R^2
plot(log(df$price), log(df$carat))
bplog <- breusch_pagan(fitlog, koenker = F)
koenkerlog <- breusch_pagan(fitlog, koenker = T) # still heavily heteroscedastic, unfortunately
# Goldfeld - Quandt test, parametric and non-parametric
gq <- goldfeld_quandt(fit, method = "parametric", alternative = "greater") # non-parametric requires Rmpfr lib
goldfeld_quandt(lm(Agriculture ~ Infant.Mortality, swiss), method = "parametric") #p.value interpretation is inversed for F-test?