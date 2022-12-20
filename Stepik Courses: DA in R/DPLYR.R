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
