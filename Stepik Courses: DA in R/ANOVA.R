# ANOVA
library(ggplot2)
# Formulae
DV ~ IV1 # One-way ANOVA
DV ~ IV1 + IV2 # Two-way ANOVA
DV ~ IV1:IV2 # Two-way Interaction ANOVA 
DV ~ IV1 + IV2 + IV:IV2 # Base formula for Two-way ANOVA : Main effects + interaction
DV ~ IV1 * IV2 # Same as #7
DV ~ (IV1 + IV2 + IV3)^2 # Main effects + all possible 2-factor interactions (3-fact interactions IV:IV2:IV3 excluded)
DV ~ IV1 + Error(subject/IV1) # Repeated measures

df <- read.csv("shops.csv", stringsAsFactors = T)

# One-way ANOVA
ggplot(df, aes(origin, price))+
  geom_boxplot()

# AOV func
fit <- aov(price ~ origin, df)
summary(fit)

# Two-way ANOVA
fit2 <- aov(price ~ origin + store, df)
summary(fit2)
# model.tables func
?model.tables
model.tables(fit2, "means")

#Interactions
ggplot(df, aes(store, price, color = origin, group = origin)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.25)+
  stat_summary(fun.data = mean_cl_boot, geom = "line")+
  stat_summary(fun.data = mean_cl_boot, geom = "point", size = 2)+
  theme_bw()

fit_int <- aov(price ~ origin + store + origin:store, df) # same as aov(price ~ origin * store, df)
summary(fit_int)

# Homework 
df <- npk
VAWI <- aov(yield ~ N*P, df)
summary(VAWI)

# Homework 2 Three-way ANOVA
VA3WI <- aov(yield ~ N + P + K, df)
summary(VA3WI)

# Pairwise comparisons

ggplot(df, aes(x = food, y = price))+
  geom_boxplot()

fit_food <- aov(price ~ food, df)
summary(fit_food)

# Tukey Honest Significant Differences
?TukeyHSD
TukeyHSD(fit_food)

# Homework 3
df <- iris
iris_VA <- aov(Sepal.Width ~ Species, df)
summary(iris_VA)
TukeyHSD(iris_VA)


# Repeated measures
df <- read.csv("https://stepik.org/media/attachments/lesson/11505/therapy_data.csv", stringsAsFactors = T)
df$subject <- as.factor(df$subject)  

# Basic One-way ANOVA
therapy_VA_basic <- aov(well_being ~ therapy, df)
summary(therapy_VA_basic)

# One-way ANOVA + Repeated Measures Error
therapy_VA_RM <- aov(well_being ~ therapy + Error(subject/therapy), df)
summary(therapy_VA_RM)

# Two-way ANOVA with interaction
therapy_VA_basic_2W <- aov(well_being ~ therapy*price, df)
summary(therapy_VA_basic_2W)
ggplot(df, aes(x = price, y = well_being))+
  geom_boxplot()

# Two-way ANOVA with interaction and Repeated Measures Error
therapy_VA_2W_RM <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), df)
summary(therapy_VA_2W_RM)
ggplot(df, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject)

# Three-way ANOVA with interaction
therapy_VA_basic_3W <- aov(well_being ~ therapy*price*sex, df)
summary(therapy_VA_basic_3W)

# Three-way ANOVA with interaction and Repeated Measures Error
therapy_VA_3W_RM <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), df)
summary(therapy_VA_3W_RM)

# Homework 4 One-Way Repeated Measures ANOVA
df <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv", stringsAsFactors = T)
df$patient <- as.factor(df$patient)
patientVA <- aov(temperature ~ pill + Error(patient/pill), df)
summary(patientVA)

# Homework 5 Two-Way Repeated Measures ANOVA
patientVA2 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), df)
summary(patientVA2)

# Homework 6 Plotting Interactions within variable
df <- ToothGrowth
obj <- ggplot(df, aes(x = as.factor(dose), y = len, color = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))