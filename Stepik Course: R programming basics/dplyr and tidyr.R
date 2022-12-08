# TIDYR AND DPLYR
# tidyr
# columns - variables, rows - observation
# Not tidy data example because 1 column has 2 different variables, T1&T2 and DrugA&DrugB
set.seed(1122)
df <- data.frame(Name = c("John", "Peter", "Mary", "Caroline"),
                 DrugA_T1 = runif(4, 35, 36),
                 DrugA_T2 = runif(4, 36, 39),
                 DrugB_T1 = runif (4, 36, 36.6),
                 DrugB_T2 = runif(4, 37, 38.5)
); df
# Tidy it with GATHER - wide-to-long transformation
library(tidyr)
df <- gather(df, Variable, Temperature, -Name)
# Separate variable into two with SEPARATE (or unite with UNITE)
df <- separate(df, Variable, c("DrugType", "Time"), "_"); df

# DPLYR
library(dplyr)
# Select - select by column - Time & Temperature
select(df, Time, Temperature)
select(df, starts_with("T"))
select(df, 3:4)
select(df, contains("Time"), Temperature)
select(df, -contains("T"))
select(df, -Name, -DrugType)
select(df, matches("me$"))
df %>% select(c(1:2, 4))
# filter - select by rows 
filter(df, Temperature > 37, Name %in% c("John", "Mary"))
# arrange - sorting
arrange(df, Name, -Temperature)
# mutate - create or change variables
mutate(df, DrugType = gsub("Drug", "", DrugType))
# summarize with groupby
summarize(group_by(df, Time), AvgTemp = mean(Temperature))
library(data.table)
# Homework 1 - select practice
data <- data.frame(first_name = c("John", "Peter", "Mary", "Caroline"),
               last_name = c("White", "Black", "Red", "Blue"),
               email_address = c("White@mail.ru", "Black@mail.ru ", "Red@mail.ru ", "Blue@mail.ru "),
               postal_address = c("Washington", "Birmingem", "Rostov", "Bratsk"),
               date_added = c("01.01.2022", "02.02.2022", "03.03.2022", "04.04.2022"))
select(data, first_name, last_name, date_added)
select(data, matches("_.{4,5}$"))
select(data, -contains("_add"))
select(data, contains("name"), date_added)
select(data, -3:4)
data %>% select(c(1:2, 5)) 
# Conveyors - operator %>% (pipe operator)

df <- data.frame(type = c(1,1,2,2,3,3), value = c(5,10,50,100,7,7))
# nested calls - hard to read
arrange(
  summarize(
    group_by(df,type),
    Total = sum(value)
  ),
  Total
)
# creating useless variables
a <- group_by(df, type)
b <- summarize(a, Total = sum(value))
c <- arrange(b, -Total)
# CONVEYOR / PIPE-ing!
df %>%
  group_by(type) %>%
  summarise(Total = sum(value)) %>%
  arrange(-Total)
# no commas and \n's, no a,b,c variables, no nesting and hence parenthesis issues, shortest.
# %>% inputs x as argument into f(), so f(x) is x %>% f, x %>% f(y) is f(x,y) - x always comes first,
# x %>% f(y, param = .) is f(y, param = x) . represents the place to put x

library(magrittr)
library(stringr)
options(stringsAsFactors = F)
# Dplyr Practice
avian <- read.csv("avianHabitat.csv")
# first approach - using base R
avian <- subset(avian, PDB > 0 & DBHt > 0, c("Site", "Observer", "PDB", "DBHt"))
avian$Site <- factor(str_replace(avian$Site, "[:digit:]+", ""))
subset(
  aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max),
  x >= 5
)

# second approach - using pipes
avian <- read.csv("avianHabitat.csv")
avian <- 
  avian %>% 
  subset(PDB > 0 & DBHt > 0, c("Site","Observer", "PDB", "DBHt")) %>% 
  transform(Site = factor(str_replace(.$Site, "[:digit:]+", "")))

aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max) %>% 
  subset(x >= 5)

# third approach - pipes and dplyr
avian <- read.csv("avianHabitat.csv")

avian %>% 
  filter(PDB > 0, DBHt > 0) %>% 
  select(Site, Observer, contains("DB")) %>% 
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>% 
  group_by(Site, Observer) %>% 
  summarize(MaxHt = max(DBHt)) %>% 
  filter(MaxHt >= 5)

# Homework 2 - warpbreaks analysis with dplyr
df <- warpbreaks
df %>%
  group_by(wool,tension) %>% 
  summarize(avg = mean(breaks), max = max(breaks)) %>% 
  filter(avg > 25 | max > 42)
# Homework 3 - The last task
library(stringr)
library(dplyr);library(tidyr);library(magrittr)
avian <- read.csv("avianHabitat.csv")
avian %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>%
  select(c(Site, Observer,contains("Ht"))) %>%
  filter(HHt > 0) %>% 
  group_by(Site, Observer, HHt > 0) %>%
  tally()

# Proper solution:
avian %>% 
  select(Site, Observer,contains("Ht")) %>% 
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>% 
  group_by(Site, Observer) %>% 
  summarise(across(contains("Ht"), function(x) sum(x>0)))