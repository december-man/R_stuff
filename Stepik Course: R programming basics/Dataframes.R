# Data import 
# from .tsv & .csv - data.table (data.table::fread), readr, read.table("clipboard")
# from unstructured text - readlines, scan
# XML, HTML - library(XML), library(httr)
# JSON, YAML - library (rjson), library (RJSONIO)
# EXCEL - library (readxl), library(XLConnect)
# SAS, Stats, SPSS, Matlab - haven, foreign, sas7bdat
# Web - rvest
# DBs, relational - DBI, RSQLite, non-relational - rmongodb
#Формально, data.frame — это именованный список (см. typeof(data.frame))
#все элементы которого имеют одинаковую длину и который имеет атрибут row.names
typeof(data.frame)
class(data.frame)
#crossover между списками и матрицами
df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T,F), stringsAsFactors = T,
                 row.names = c("Alpha", "Bravo", "Charlie", "Delta"))
str(df)
dim(df); nrow(df); ncol(df)
# names(df) == colnames(df)
# length(df) == ncol(df)
# indexation is the same as for the matrices
df[3:4,-1];df[, 1, drop = F]
df[df$x > 2,]
subset(df, x > 3)
subset(df, x > 1, select = c(x,z))

#rbind & cbind (as in R in DA)

# Merge function (INNER JOIN in SQL)
df_salary <- data.frame(x=c(3, 2, 6, 1), salary=c(100, 1000, 300, 500))
dfm <- merge(df,df_salary, by = 'x')
#Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
#Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
#Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
#Cross join: merge(x = df1, y = df2, by = NULL)

#Homework 1-3
str(df)
typeof(as.matrix(df))
class(as.matrix(df))
str(attitude)
dfa <- attitude
selected_dfa <- dfa[order(-dfa$learning),][1:5,c("complaints","raises","advance")]
names(which.max(rowSums(selected_dfa)))

# D A T A C L E A N I N G 
#Homework
attitude[attitude$rating < 50, names(attitude) != "rating"]
attitude[attitude$rating < 50, -"rating"]
attitude[rating < 50, names(attitude) != "rating"]
subset(sel = -rating, sub = rating < 50, attitude)
subset(attitude, rating < 50, -rating)
?quakes
summary(quakes)

#Binding two data frames together (second one is naughty)
df <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv",
               stringsAsFactors = T)
names(df); str(df); summary(df); View(df)
coverage_variables <-  names(df)[-(1:4)][c(T,F)]
df$total_coverage <- rowSums(df[, coverage_variables])
summary(df$total_coverage)

df2 <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat2.csv",
                stringsAsFactors = T, skip = 5, sep = ";",
                na.strings = "Don't remember", dec = ".", comment.char = '%')
df2$Observer <- "KL"
df2 <- df2[, colnames(df2)[c(1,17,2:16)]]
names(df2); View(df2)
coverage_variables2 <-  names(df2)[-(1:4)][c(T,F)]
df2$total_coverage <- rowSums(df2[, coverage_variables2])

#rbind
df3 <- rbind(df,df2)
summary(df3$total_coverage); summary(df$total_coverage)

#Homework 4
m <- sort(sapply(df[, colnames(df)[seq(6, 16, 2)]], max), T)
