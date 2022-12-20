# Shapiro-Wilk test idea
library(dplyr)
library(ggplot2)
samples_number <- 10000
sample_size <- 3
d <- data.frame(x = rnorm(sample_size*samples_number), 
                sample = rep(1:samples_number, each=sample_size)) 
# get 3 ranged (min-mid-max) values out of given distribution
# do that 10000 times, if those values will be distributed like 3 gaussian curves - your distribution is normal!
grouped_data <- group_by(d, sample)
grouped_data <- arrange(grouped_data, x, .by_group = T)
grouped_data$n <- rep(1:sample_size, samples_number)

ggplot(grouped_data, aes(x, fill = factor(n)))+
  geom_density()
