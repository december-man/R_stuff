"LMER with LME4"
library(mlmRev); library(lmerTest); library(lme4); library(ggplot2)
df <- Exam

ggplot(Exam, aes(x = standLRT, y = normexam, col = school))+
  geom_point(size = 0.1)
# basic lm
fit1 <- lm(normexam ~ standLRT, df)
summary(fit1) 
df$fitpred <- predict(fit1)

ggplot(df, aes(x = standLRT, y = normexam))+
  geom_point(size = 0.05)+
  geom_line(data = df, aes(x = standLRT, y = fitpred), col = 'blue')+
  geom_smooth(data = df, aes(x = standLRT, y = normexam), method = 'lm')
# lm with mixed effects
# lm with fixed effect + 1 random effect (intercept only)
# REML - Restricted Maximum Likelihood
fit2 <- lmer(normexam ~ standLRT + (1|school), df)
summary(fit2)
df$fitpredlmer <- predict(fit2)
ggplot(df, aes(standLRT, normexam))+
  geom_point(size = 0.1)+
  geom_line(data = df, aes(standLRT, fitpredlmer, col = school))
# LMER with 1 fixed effect, 1 random effect (slope + intercept)
fit3 <- lmer(normexam ~ standLRT + (standLRT + 1|school), df)
summary(fit3)
df$firpredlmer2 <- predict(fit3)
ggplot(df, aes(standLRT, normexam))+
  geom_point(size = 0.1)+
  geom_line(data = df, aes(standLRT, firpredlmer2, col = school))


# LMER with 1 fixed effect, 1 random effect (only slope)
fit4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), df)
summary(fit4)
df$fitpredlmer3 <- predict(fit4)
ggplot(df, aes(standLRT, normexam))+
  geom_point(size = 0.1)+
  geom_line(data = df, aes(standLRT, fitpredlmer3, col = school))


# LMER with 1 fixed effect, 1 random effect(slope+intercept, uncorrelated)
fit5 <- lmer(normexam ~ standLRT + (0 + standLRT|school) + (1|school), df)
summary(fit5)

#lmerTest

fitt <- lmer(normexam ~ standLRT + (1|school), data = Exam)
summary(fitt)

# GLMER - generalized LMER
# Logistic LMER
df$school_type <- ifelse(df$type == 'Mxd', 1, 0)
fit6 <- glmer(school_type ~ normexam + (1|school), family = 'binomial', df)
summary(fit6)
# Prediction on a new dataset
new_df <- df[sample(1:nrow(df), 100),]
new_df$school <- sample(101:200)
fit2 <- lmer(normexam ~ standLRT + (1|school), df, REML = F)
predict(fit2, new_df, allow.new.levels = T)

# lme4 fixed effects and random effects shortcut
fixef(fit3)
ranef(fit3)

# HOMEWORK 1-5

# homework 1, ggplotting boxplot
library(ggplot2)
df <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv", stringsAsFactors = T)
ggplot(df, aes(x = factor(scenario), y = frequency, fill = attitude))+
  geom_boxplot()
# homework 2, ggplotting density
ggplot(df, aes(x = frequency, fill = subject))+
  geom_density()+
  facet_grid(gender~.)
# homework 3, lme4
library(lme4)
fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), df)
summary(fit_1)
# homework 4, lme4 2
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), df)
summary(fit_2)
# homework 5, lme4 3
fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario), df)
summary(fit_3)