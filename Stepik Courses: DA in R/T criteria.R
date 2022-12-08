# Student's criteria
df <- iris
dfr <- subset(df, Species != "setosa")
library(ggplot2)

ggplot(dfr, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.4) +
  facet_grid(Species ~ .)

ggplot(dfr, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.2)

ggplot(dfr, aes(Species, Sepal.Length)) +
  geom_boxplot()

#Shapiro-Wilk to test normality of distribution
shapiro.test(dfr$Sepal.Length)
shapiro.test(dfr$Sepal.Length[dfr$Species == 'versicolor'])
shapiro.test(dfr$Sepal.Length[dfr$Species == 'virginica'])
#using by() function to calculate values of shapiro-wilk's test for all the factors in "Species"
by(dfr$Sepal.Length, INDICES = dfr$Species, shapiro.test)

# Bartlett homogeneity of variances test. ~ is a formula sign meaning  "evaluate by",
# Species has to be a "factor" of two for a two sample t-test
bartlett.test(Sepal.Length ~ Species, dfr)

# T.test
# Welch's Two sample T-test
Ttest <- t.test(Sepal.Length ~ Species, dfr)
# Minding homogeneity
Ttest <- t.test(Sepal.Length ~ Species, dfr, var.equal = T)
# One sample T-test, compare mean values in sample and population, mu - mean value in GP
Ttest <- t.test(dfr$Sepal.Length, mu = 6.261)
# T-test for paired vectors (dependent samples)
Ttest <- t.test(dfr$Petal.Length, dfr$Petal.Width, paired = T)

# Wilcoxon test (non-parametric T-test analog), same is Mann-Whitney U-statistics
# Wilcoxon rank sum test with continuity correction
(Wtest <- wilcox.test(Petal.Length ~ Species, dfr))
# For paired vectors - Wilcoxon signed rank test
(Wtest <- wilcox.test(dfr$Petal.Length, dfr$Petal.Width, paired = T))

# Homework
df <- ToothGrowth
t_stat <- t.test(df$len[df$supp == "OJ" & df$dose == 0.5], df$len[df$supp == "VC" & df$dose == 2.0])$statistic

# Homework 2
df <- read.csv("https://stepic.org/media/attachments/lesson/11504/lekarstva.csv")
t_stat2 <- t.test(df$Pressure_before, df$Pressure_after, paired = T)

# Mean and confidence intervals in ggplot2
ggplot(dfr, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)
# or
ggplot(dfr, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1)

#Boxplotting
ggplot(dfr, aes(Species, Petal.Length))+
  geom_boxplot()

#Homework - 5 minutes to get means comparison (1 numerical & 1 factor variable)
getwd()
df <- read.table("df2.txt") #reading data
bartlett.test(V1 ~ V2, df) # vars are homogenic (p = 0.4 > 0.05)
t.test(V1 ~ V2, df, var.equal = T) #since variances are homogenic, we can use Ttest, otherwise U-test)

#Homework 2 - 5 minutes to get means comparison (2 numerical variables)
df <- read.table("data.txt")
t.test(df$V1, df$V2, var.equal = F)
