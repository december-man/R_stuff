# Correlation analysis and basic Linear regression model
df <- mtcars

# Basic correlation test function with non-parametric methods available
corr_mpg_hp <- cor.test(x = df$mpg, y = df$hp, method = "pearson")
plot(df$hp, df$mpg)
str(corr_mpg_hp)
corr_mpg_hp$p.value
corr_mpg_hp$estimate

# formula description #all variables are independent and numeric, hence the formula look
corr_mpg_hp <- cor.test( ~ mpg + hp, df)

# ggplotting
library(ggplot2)
ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point()

# pairs function
df_numeric <- df[, c(1,3:7)]
pairs(df_numeric)

# cor function
cor(df_numeric)

# psych's corr.test
library(psych)
corr_multiple <- corr.test(df_numeric)
corr_multiple

# Homework 1
corr.calc <- function(x) {
  corr <- cor.test(~ x[[1]] + x[[2]], x, method = 'pearson')
  return(c(corr$estimate, corr$p.value))
}

# Homework 2 - filtered.cor
data <- read.csv("https://stepic.org/media/attachments/lesson/11504/step6.csv", stringsAsFactors = T)
library(psych)
filtered.cor <- function(data) {
  corr <- corr.test(data[sapply(data, is.numeric)])
  diag(corr$r) <- 0
  return(corr$r[which.max(abs(corr$r))])  
}; filtered.cor(data)

# Homework 3 - Normality test condition
smart_cor <- function(df){
  cor.test(df[[1]], df[[2]], method = ifelse(any(sapply(df, function(x) shapiro.test(x)$p.value) < 0.1), "sp", "pe"))$p.value
}

# lm function and basic linear regression model
df <- mtcars
df_numeric <- df[sapply(df, is.numeric)]
fit <- lm(mpg ~ hp, df)
summary(lm(mpg ~ hp, df))

# plotting trend line
ggplot(df, aes(x = hp, y = mpg))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", SE = F)+
  facet_grid(.~cyl)

# fitted values (model prediction)
fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

# predict function
new_hp <- data.frame(hp = c(100, 150, 129, 300))
predict(fit, new_hp)
new_hp$mpg <- predict(fit, new_hp)
# Linear regression on nominal variable (logistic regression) - much like multiple ANOVA
df$cyl <- factor(df$cyl, labels = c("four", "six", "eight"))
fit <- lm(mpg ~ cyl, df)
summary(fit)

ggplot(df, aes(x = cyl, y = mpg))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

aggregate(mpg ~ cyl, df, mean)

# Homework 4 - 5 minutes to get basic LR
df <- read.table("data.txt")
fit <- lm(V1 ~ V2, df)
summary(fit)
fit$coefficients

# Homework 5 
df <- diamonds
data <- subset(df, cut == "Ideal" & carat == 0.46)
fit <- lm(price ~ depth, data)
fit_coef <- fit$coefficients

# Homework 6 - regr.calc - function that firstly checks correlation then builds LRM
regr.calc <- function(x) {
  if (cor.test(~ x[[1]] + x[[2]], x)$p.value < 0.05) {
    x$fit <- lm(x[[1]] ~ x[[2]])$fitted.values; x
  } else {
    "There is no sense in prediction"
  }
}

# Homework 7 aes() in ggplot() will be forwarded into all the geoms
library(ggplot2)
df <- iris
ggplot(df, aes(x = Sepal.Width, y = Petal.Width, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")

# coin's Spearman test is used to avoid problems with having the same values in 2 vectors 
library(coin)
spearman_test(~ mtcars$mpg + mtcars$hp, mtcars)
cor.test(~ mtcars$mpg + mtcars$hp, mtcars, method = 'spearman')

# olsrr package for Homework #9 for finding the best predictor combination in LR for max R-squared
library(olsrr)

# Multiple LR
swiss <- data.frame(swiss)
hist(swiss$Fertility, col = 'red')

# syntax is the same as in ANOVA
fit <- lm(Fertility ~ Examination + Catholic, swiss)
summary(fit)
fit2 <- lm(Fertility ~ Examination * Catholic, swiss)
summary(fit2)

# building confidence intervals (missing in summary)
confint(fit2)

# Homework 8 - fill "NA" values with LRM data values
fill_na <- function(data) {
  data$y_full <- predict(lm(y ~ x_1 + x_2, data), data)
  data$y_full[which(!is.na(data$y))] <- data$y[which(!is.na(data$y))]; data
}

# Homework 9 - finding optimal number and combination of predictors for LRM - using olsrr tools
library(olsrr)
data <- mtcars[, c(1, 3:6)]
fit <- lm(wt ~ ., data)
ols_step_best_subset(fit)

# Homework 10 
df <- attitude
fit <- lm(rating ~ complaints*critical, df)
summary(fit)

# MLR with categorial predictors
# Creating one
swiss$religious <- ifelse(swiss$Catholic > 60, "Lots", "Few")
swiss$religious <- as.factor(swiss$religious)
fit <- lm(Fertility ~ Examination + religious, swiss)
summary(fit)
# Intercept has the values for the religious "Few" group (few Catholics), not just y0
# MLR with CP + interaction
fit <- lm(Fertility ~ religious * Examination, swiss)
summary(fit)
# Sometimes it's better to put categorial variable in the first place in the lm formula, easier to interprete results

# ggplotting
ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# adding infant mortality
fit <- lm(Fertility ~ religious * Infant.Mortality * Examination, swiss)
summary(fit)

# Homework 11
df <- mtcars
df$am <- factor(df$am, labels = c("Automatic", "Manual"))
fit <- lm(mpg ~ am*wt, df)
summary(fit)

ggplot(df, aes(x = wt, y = mpg, col = am)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# MLR, model comparison
rm(swiss)
swiss <- data.frame(swiss)
fit_full <- lm(Fertility ~ ., swiss)
summary(fit_full)
fit_no_agcult <- lm(Fertility ~ . -Agriculture, swiss)
summary(fit_no_agcult)
fit_no_exam <- lm(Fertility ~ . -Examination, swiss)
# comparison of 'R-squared' from 2 MLR models fit_full and fit_no_agcult using ANOVA
anova(fit_full, fit_no_agcult)
anova(fit_full, fit_no_exam)
# model selection using olssr and base r
ols_step_best_subset(lm(Fertility ~., swiss))
optimal_fit <- step(fit_full, direction = 'backward')

# Homework 12
fit_null <- lm(Fertility ~ 1, swiss)
fit_full <- lm(Fertility ~ ., swiss)
ideal_model <- step(fit_full, scope = list(lower = fit_null, upper = fit_full),  direction = 'backward')

# Homework 13
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
ideal_model <- step(model_full, scope = list(lower = model_null, upper = model_full),  direction = 'backward')
anova(ideal_model, model_full)

# Homework 14
# the tits!
model <- lm(sr ~ (.)*(.), LifeCycleSavings)
summary(model)

# MLR diagnostics, linearisation and normalisation of data
?scale

# Homework 15 normalisation using sqrt
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(sqrt(my_vector))

# Homework 16 scale func - perform Z-normalisation of input data and get LR coefficients
z <- scale(mtcars[,c(1,3)])
attr(z, "scaled:center")
beta.coef <- function(x) {
  x <- scale(x)
  lm(x[[1]] ~ x[[2]], x)$coefficients
}

# Homework 17 - normality test before MLR analysis
normality.test <- function(x) {
  sapply(x, function(x) shapiro.test(x)$p.value) 
}
normality.test(mtcars[,1:6])

# Linearity and residuals analysis

library(ggplot2)
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth()
lm1 <- lm(Education~Examination, swiss)
summary(lm1)

#notice quadratic law of dependency between this two factors
swiss$Examination_squared <- (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

# Examination_sqaured predicts Education better because of the quadratic nature of dependency
ggplot(swiss, aes(Examination_squared, Education))+
  geom_point()+
  geom_smooth()

anova(lm2,lm1)
# significant difference between two models!

swiss$lm1fitted <- lm1$fitted.values
swiss$lm2fitted <- lm2$fitted.values
swiss$lm1resid <- lm1$residuals
swiss$lm2resid <- lm2$residuals
swiss$obs_number <- 1:nrow(swiss)
# graphical comparison
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_line(aes(Examination, lm1fitted), col = "red", lwd = 1)+
  geom_line(aes(Examination, lm2fitted), col = "blue", lwd = 1)
# residuals analysis
ggplot(swiss, aes(lm1fitted, y = lm1resid))+
  geom_point(size = 1)+
  geom_hline(yintercept = 0, col = 'red', lwd = 1)

ggplot(swiss, aes(lm2fitted, y = lm2resid))+
  geom_point(size = 1)+
  geom_hline(yintercept = 0, col = 'red', lwd = 1)

# independence of errors check
ggplot(swiss, aes(x = obs_number, y = lm1resid))+
  geom_point(size = 1) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2resid))+
  geom_point(size = 1) + geom_smooth()

# homoscedasticity check
ggplot(swiss, aes(lm1fitted, y = lm1resid))+
  geom_point(size = 1)

ggplot(swiss, aes(lm2fitted, y = lm2resid))+
  geom_point(size = 1)

# gvlma() LM vulnerabilities check
install.packages("gvlma")
library(gvlma)
# Homework 18 gvlma()
df <- read.csv("https://stepic.org/media/attachments/lesson/12088/homosc.csv")
fit <- lm(DV ~ IV, df)
x <- gvlma(fit)

# normality of residuals distribution
ggplot(swiss, aes(x = lm1resid))+
  geom_histogram(binwidth = 2, fill = "white", col = "brown")
qqnorm(lm1$residuals)
qqline(lm1$residuals)
shapiro.test(lm1$residuals)

ggplot(swiss, aes(x = lm2resid))+
  geom_histogram(binwidth = 2, fill = "white", col = "brown")
qqnorm(lm2$residuals)
qqline(lm2$residuals)
shapiro.test(lm2$residuals)

# Homework 19 normality of rd and ggplotting
resid.norm <- function(fit) {
  nord <- shapiro.test(fit$residuals)$p.value
  ggplot(swiss, aes(x = fit$residuals))+
    geom_histogram(binwidth = 2, fill = ifelse(nord > 0.05, "green", "red"))
}

# Homework 20 multicollinearity checker
library(psych)
high.corr <- function(x){
  corr_matrix <- corr.test(x)$r 
  corr_matrix <- abs(corr_matrix - diag(ncol(corr_matrix)))
  return(rev(dimnames(which(corr_matrix == max(abs(corr_matrix)), arr.ind = T))[[1]]))
}

# Logistic regression
# probability > odds > ln of odds (logit)
df <- read.csv("https://stepic.org/media/attachments/lesson/10226/train.csv", sep = ";", stringsAsFactors = T)
ggplot(df, aes(read, math, col = gender, size = 1))+
  geom_point()+
  facet_grid(.~hon)+
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 9, face = "bold"))

fit <- glm(hon ~ read + math + gender, df, family = "binomial")
summary(fit)
fit$coefficients

exp(fit$coefficients)
#getting back to linear model
df$prob <- (predict(fit, type = "response")) 
# response returns probabilities

# Homework 21 mtcars glm
log_coef <- glm(am ~ disp + vs + mpg, mtcars, family = "binomial")$coefficients

# Homework 22 boxplotting 
ggplot(data = ToothGrowth, aes(x = supp, y = len))+
  geom_boxplot(aes(fill = as.factor(dose)))

# ROCR
install.packages("ROCR")
library(ROCR)
#prediction function
pred_fit <- prediction(df$prob, df$hon)
#performance function
perf_fit <- performance(pred_fit, "tpr", "fpr")
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0, 1, by = 0.1))
AUC <- performance(pred_fit, "auc")
str(AUC)
#specificity (same as true negative rate), sensitivity and accuracy of a classificator
perf_spec <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf_sens <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf_acc <- performance(pred_fit, x.measure = "cutoff", measure = "acc")
plot(perf_spec, col = 'red', lwd = 2)
plot(add = T, perf_sens, col = 'green', lwd = 2)
plot(add = T, perf_acc, col = "black", lwd = 2)
legend(x = 0.3, y = 0.5, c("spec", "sens", "acc"),
       lty = 1, col = c("red", "green", "black"), bty = "n", cex = 1, lwd = 1)
abline(v = 0.220, lwd = 1)
# prediction and real data comparison
df$pred_resp <- factor(ifelse(df$prob > 0.225, 1, 0), labels = c("N", "Y"))
df$correct <- ifelse(df$pred_resp == df$hon, 1, 0)

ggplot(df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 9, face = "bold"))
# prediction accuracy - 77%
mean(df$correct)

# Homework 23 - The graduation
df <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv", stringsAsFactors = T)
df$admit <- as.factor(df$admit); df$rank <- as.factor(df$rank)
dfNA <- subset(df, is.na(admit))
fit <- glm(admit ~ rank*gpa, df, na.action = na.exclude, family = "binomial")
summary(fit)
dfNA$prob <- predict(fit, type = "response", newdata = dfNA)
nrow(dfNA[dfNA$pred_resp == "Y",])
