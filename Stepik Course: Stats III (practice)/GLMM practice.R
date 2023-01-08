"Understanding Linear Mixed Effects Models (LMER)"
"https://ourcodingclub.github.io/tutorials/mixed-models/"

load("dragons.RData")
head(dragons)
 hist(dragons$testScore)  # seems close to a normal distribution - good!
# Centering and scaling our data
# scale() centers the data (the column mean is subtracted from the values in the column) and
# then scales it (the centered column values are divided by the column’s standard deviation)
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

library(ggplot2); library(dplyr); library(tidyr)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1)  # not perfect... 
# but since this is a fictional example we will go with it
# for your own data be careful:
# the bigger the sample size, the less of a trend you'd expect to see
plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad
# What about data indeperendence?
# We collected multiple samples from eight mountain ranges. 
# It’s perfectly plausible that the data from within each mountain range are more similar to each other than 
# the data from different mountain ranges: they are correlated.
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here
(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))
# Let's fit a regression model within all mountainRange clusters respectfully
(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() + 
    facet_wrap(~ mountainRange) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))
# Minding we also have Sites inside mountainRanges - thats 24 different models!
# We are rapidly decreasing the sample size (up to N = 20)
# We increase a chance of type I error (where you falsely reject the null hypothesis) by carrying out multiple comparisons.
# One solution is to add mountainRange as a fixed effect
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)
# BUT! we are not interested in quantifying test scores for each specific mountain range: 
# we just want to know whether body length affects test scores 
# and we want to simply !control! for the variation coming from mountain ranges
# This is where LMER comes in - mountainRange is a "random factor"
library(lme4)
# Estimate fewer parameters and avoid problems with multiple comparisons 
# that we would encounter while using separate regressions.
# Higher sampler size with account for correlations between data from sites and mountainRanges.
# Random effects are usually grouping factors for which we are trying to control. They are always categorical
# with at least 5 levels for smaller confidence intervals. If your factor has <5 levels - include it as a fixed effect
# Note that our question changes slightly here: while we still want to know whether there is an association 
# between dragon’s body length and the test score, we want to know if that association exists after controlling for 
# the variation in mountain ranges.
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
# Mountain ranges are clearly important: they explain a lot of variation. 
# How do we know that? We can take the variance for the mountainRange and divide it by the total variance:
339.7/(339.7 + 223.8)  # ~60 %
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!
# We can potentially observe every dragon in every mountain range (crossed random effect)
# or at least observe some dragons across some of the mountain ranges (partially crossed RE)
# Nested random effects are self-explanatory.
# Implicit and explicit nesting. Avoid the first. 3 sites in 8 different mountainRanges means 24 different places
# since site A at mountainRange 1 has nothing to do with the site A in any other different mountainRange -> remove Sites.
# if random effects aren't nested, they are most likely crossed.
dragons <- within(dragons, sample <- factor(mountainRange:site))
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  
# the syntax stays the same, but now the nesting is taken into account, also same as (1|mountainRange/site)
summary(mixed.lmer2)
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(size = 0.1) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 0.5) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)
# This was a random-intercept model, since we didn't mention the variation of testcore~bodylength behavior in diff mountain ranges
# Let's fit a random-slope model
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 
summary(mixed.ranslope)
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(size = 0.1) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)
# Prediction with LMER, Data extraction
# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
  theme_minimal()

library(stargazer)
stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
# P-value, model selection
# rule of thumb - you need 10 times more data than parameters you are trying to estimate.
# Worst to best:
# Wald Z-tests
# Wald t-tests (but LMMs need to be balanced and nested)
# Likelihood ratio tests (via anova() or drop1())
# MCMC or parametric bootstrap confidence intervals
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)
# REML set to FALSE because we have to use ML since our models have different fixed effects
# Even though we use ML to compare models, we should report parameter estimates from your final “best” REML model,
# as ML may underestimate variance of the random effects.
# “Two models with nested random structures cannot be compared 
# with ML because the estimators for the variance terms are biased.”
anova(reduced.lmer, full.lmer)  # the two models are not significantly different
# Models can also be compared using the AICc function
# The Akaike Information Criterion (AIC) is a measure of model quality. 
# AICc corrects for bias created by small sample size when estimating AIC.
# Generally, if models are within 2 AICc units of each other they are very similar. 
# Within 5 units they are quite similar, over 10 units difference - go with the model with lower AICc.
# There isn’t really an agreed upon way of dealing with the variance from the random effects in mixed models
# when it comes to assessing significance. 
# Both p-values and effect sizes have issues.
library(AICcmodavg)
AICc(reduced.lmer)
AICc(full.lmer)
# Do NOT vary random and fixed effects at the same time - either deal with your random effects structure or
# with your fixed effects structure at any given point.
# Do NOT compare lmer models with lm models (or glmer with glm).
# Model selection , Zuur et al:
# 1. Fit a full model that is 'beyond optimal'
# 2. Sort out the random effects structure (use REML likelihoods or REML AIC or BIC)
# 3. Sort out fixed effects structure 
# (either use REML the F-statistic or the t-statistic or compare nested ML models - keep your random effects constant)
# 4. Once you arrive at the final model present it using REML estimation