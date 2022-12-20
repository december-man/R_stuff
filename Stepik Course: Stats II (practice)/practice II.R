library(ggplot2); library(dplyr); library(tidyr); library(magrittr)
# Stepic Stats II:  practice II
# Homework 1: logistic regression with one categorial predictor
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
get_coefficients <- function(df){
  df <- mutate(df, x = factor(x), y = factor(y))
  exp(glm(y ~ x, df, family = "binomial")$coefficients)
}

# Homework 2: centering variables in dataframe 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
centered <- function(df, vars){
  df[, vars] <- scale(df[, vars], scale = F); df
}

# Homework 3: return only statistically significant predictors in LRM without interactions
df <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv", stringsAsFactors = T)
get_features <- function(df){
  p <- anova(glm(is_prohibited ~ ., df, family = "binomial"), test = "Chisq")
  if (length(p$`Pr(>Chi)`[p$`Pr(>Chi)` < 0.05], na.rm = T) == 1) {return("Prediction makes no sense")}
  return(rownames(p[which(p$`Pr(>Chi)` < 0.05), ]))
}

# Homework 4: do predictions: Find out who is the person to be most suspicious of in a new df called predict
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv", stringsAsFactors = T)
predict_data <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv", stringsAsFactors = T)
most_suspicious <- function(learn, predict){
  fit <- glm(is_prohibited ~ ., learn, family = "binomial")
  predict$prob <- predict(fit, predict, type = "response")
  predict$passangers[which.max(predict$prob)]
}

# Homework 5: from DA in R II: pass Shapiro-Wilk test onto all the variables in a df
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv", stringsAsFactors = T)
normality_test <- function(df) sapply(df[sapply(df, is.numeric)], function(x) shapiro.test(x)$p.value)

# Homework 6: Perform Anova parametrically or non-parametrically, depending on homoscedasticity and normality
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv", stringsAsFactors = T)
smart_anova <- function(df){
swt <- aggregate(x ~ y, df, function(x) shapiro.test(x)$p.value)
btt <- bartlett.test(x ~ y, df)
if (any(swt$x < 0.05) | btt$p.value < 0.05) {
  KW_p <- kruskal.test(x ~ y, df)$p.value
  names(KW_p) <- "KW"; KW_p
  } else { 
  ANOVA_p <- summary(aov(x ~ y, df))[[1]]$'Pr(>F)'[1]
  names(ANOVA_p) <- "ANOVA"; ANOVA_p
  }
}

# Homework 7: calculate shapiro-wilk test p values for a given dataframe
library(dplyr)
d <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv", stringsAsFactors = T)
normality_by <- function(d) aggregate(x ~ ., d, function(x) shapiro.test(x)$p.value) %>% rename(p_value = x)

# Homework 8: ggplotting
library(ggplot2)
ggplot(iris, aes(Sepal.Length, fill = Species))+
  geom_density()
