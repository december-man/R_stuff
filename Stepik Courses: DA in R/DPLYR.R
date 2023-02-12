# Data handling with DPLYR
# difference between data.frame and data_frame(dplyr). Basically it makes all the base r data handling 
# functions look cleaner and more comfortable to work with:
library(dplyr)
library(ggplot2)
my_data <- data_frame(x = rnorm(10000), y = rnorm(10000),
                      f = factor(rep(1:2, 5000)))
my.data <- data.frame(x = rnorm(10000), y = rnorm(10000),
                      f = factor(rep(1:2, 5000)))
diamonds <- as.data.frame(diamonds)
diamonds <- as_data_frame(diamonds)
my_data_2 <- data_frame("My var" = rnorm(10))
my.data.2 <- data.frame("My var" = rnorm(10)) # creates a dot in "My.var"
# hands-on work with arguments
my_data_2 <- data_frame(x = rnorm(10), y = abs(x))
my.data.2 <- data.frame(x = rnorm(10), y = abs(x)) # x not found
# dplyr select (column selector) vs base R (select is much more flexible)
select(diamonds, cut)
diamonds$cut  

select(diamonds, cut:depth)
select(diamonds, 2,3,4,5)
diamonds[c(2:5)]

select(diamonds, -cut)
diamonds[-2]

#regex support: select(df, ...) - starts_with(), ends_with(), contains(), matches(), num_range(), all_of(), any_of()
select(diamonds, starts_with("c"))
select(diamonds, ends_with("t"))
select(diamonds, contains("t"))
select(diamonds, matches("cut"))

# dplyr row selector - slice, also slice_head(), slice_tail(), slice_sample(), slice_min/max()
slice(diamonds, 1,3,5:10)
diamonds[c(1,4,5),]
slice_max(diamonds, order_by = carat)
slice_head(diamonds)
slice_sample(diamonds, prop = 1/1000)

# dplyr row selector using column values - filter
filter(diamonds, carat > 5, color == "J")
diamonds[diamonds$carat > 5 & diamonds$color == "J", ]
subset(diamonds, carat >5 & color == "J")
filter(diamonds, carat > 4 & color == "J")

# dplyr sorting with arrange func
arrange(diamonds, -price, depth)
diamonds[order(-diamonds$price, diamonds$depth), ]

# rename df column
rename(diamonds, new_cut = cut, new_carat = carat)
names(mtcars)[c(1,4)] <- c("new1", "new2")

# Homework 1 select odd rows
slice(diamonds, seq(1, nrow(diamonds), by = 2))

# Homework 2 using pipes, do a specific data selection from mtcars
my_df <- mtcars %>% 
  select(mpg, am, vs, hp) %>% 
  filter(mpg > 14 & hp > 100) %>% 
  arrange(-mpg) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)

# mutate - modify your data to your liking - from data preprocessing to model fitting
# Homework 1,2 tweaking df with mutate
library(tidyverse) # this is the sh1t!
t <- as.data.frame(list(V1 = c(-0.1, 1.3, -1.5, 0, 0.5), V2 = c("A", "A", "B", "A", "B")))
df <- as_tibble(matrix(rnorm(30), ncol =5))
df$V6 <- c("A", "B", "B", "B", "A", "A")
mutate(V4 = round(V4))
mutate_all(df, factor)
logtrans <- function(x) log((x - min(x)) / (max(x) - min(x))  +1)
logtransform <- function(t) t %>% mutate(across(where(is.numeric), ~ log((. - min(.)) / (max(.) - min(.))  + 1)))
# With deprecated mutate_if
t %>% mutate_if(is.numeric, ~log((. - min(.))/(max(.) - min(.)) + 1))

# GROUP BY and SUMMARISE
gr_d <- group_by(diamonds, cut)
slice(gr_d, 1)
slice(diamonds, 1)
sample_n(gr_d, 3)

summarise(mtcars, mean(disp))
aggregate(mtcars, by = mtcars[2], mean)
summarise(mtcars, mean_disp = mean(disp), sd_disp = sd(disp), median_disp = median(disp))
summary(mtcars)
summarise(gr_d, mean(price))
summarise(group_by(mtcars, cyl, am), 
          mean_disp = mean(disp), 
          sd_disp = sd(disp), 
          number_of_obs = n(),)
summarise_all(group_by(mtcars, am, vs),
              sd)
# Homework 3, Descriptive statistics in dplyr
df <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")
length(df[is.na(df)])
# using across syntax
group_by(df, gender, country) 
df%>% 
  summarise(across(salary, list(
                                n = ~n(),
                                mean = mean, 
                                sd = sd, 
                                median = median, 
                                first_quartile = ~quantile(., na.rm = T)[2],
                                third_quartile = ~quantile(., na.rm = T)[4],
                                na_values = ~sum(is.na(.))
                                ), 
                   .names = "{.fn}", na.rm = T)
            )
# using just summarise
group_by(df, gender, country) %>% 
  summarise(n = n(),
            mean = mean(salary, na.rm = T), 
            sd = sd(salary, na.rm = T), 
            median = median(salary, na.rm = T), 
            first_quartile = quantile(salary, probs = 0.25, na.rm = T),
            third_quartile = quantile(salary, probs = 0.75, na.rm = T),
            na_values = sum(is.na(salary))
            )

# Homework 4 convert to factor specific columns, given by a vector of their number
t <- mtcars[1:4] 
f <- c(1,3)
to_factors <- function(t, f) t %>% mutate(across(all_of(f), ~as.factor(ifelse(. > mean(.), 1, 0))))
#old fashioned apply
to_factors <- function(t, f) {t[f] <- lapply(t[f], function(x) as.factor(ifelse(x > mean(x), 1, 0)));return(t)}
# Homework 5: group by, summarize, arrange
high_price <- diamonds %>% 
  select(color,price) %>% 
  group_by(color) %>% 
  slice_max(price, n = 10) %>% 
  print(n = 71) #71! because of price doubling
# 2nd way
high_price <- diamonds %>% 
  select(color,price) %>% 
  group_by(color) %>%
  arrange(desc(price)) %>% 
  slice_head(n = 10) # 70
# 3rd way
high_price <- diamonds %>% 
  select(color,price) %>% 
  group_by(color) %>%
  arrange(color,desc(price)) %>% 
  slice(1:10) # 70