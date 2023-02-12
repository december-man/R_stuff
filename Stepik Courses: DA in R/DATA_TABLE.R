# DATA TABLE
library(data.table)
#using readr to open given archived db entities 
library(tidyverse);library(data.table);library(dtplyr)
df <- read_csv2('dataset_5.zip')
glimpse(df)
spec(df)

# data.table's FREAD
products <- fread('products.csv')
# syntax differences
products[1:10,]
products[price > 10000 & brand %in% c('Lenovo', 'Apple')]
products[available == TRUE, ]
# significant difference between data.table and base R is [rows, columns, by, with]
products[3]
products[3,]
products[!(10:10000)]

products[, list(name = name, price.1k = price/1000)]
products[order(price, decreasing = T), list(name = name, price.1k = price/1000)][1:5]
# . = list
products[, .(name,price)]
products[order(-price), .(name = head(name), price = head(price))]
# turn off data.table syntax
products[ , c("name", "price"), with = F]
# aggregation & sorting x[i,j,by,with = T]
# SELECT j FROM x WHERE i GROUP BY by
products[, (price = sum(price))]
products[, .(mean.price = mean(price)), by = brand]
products[order(-price), .(name = head(name, 3), price = head(price, 3)), by = brand]
# adding new operation with new set of [] 
products[, .(name.with.brand = paste0(brand, " - ", name))][order(name.with.brand)]
# complex evaluation with {} inside []
products[, .(price = {
  a <- mean(price)
  b <- median(price)
  c(min(price), max(price), a/b)})]

# Homework data.table 1-3
# Homework 1
filter.expensive.available <- function(df, brands) {
  df <- as.data.table(df)
  df[price >= 5e+03 & available == T & brand %in% brands][, .(price, brand, available)]
}
filter.expensive.available(products, c("Lenovo", "Gefest"))

# Homework 2
purchases <- fread('purchases.csv')
glimpse(purchases)
ordered.short.purchase.data <- function(df) {df[quantity > 0][order(-totalcents), .(ordernumber, product_id)]}
ordered.short.purchase.data(purchases)

# Homework 3
purchases.median.order.price <- function(df) {
  median(df[quantity > 0][, .(price = sum(totalcents)), by = ordernumber][, price])}
purchases.median.order.price(purchases)

# DATA GROUPING AND JOIN with data.table
# Subset of Data (SD)
# products[order(-price), .(name = head(name, 3), price = head(price, 3)), by = brand]
products[order(-price), head(.SD, 3), by = brand]
# Number of elements in group .N
products[price > 1000, .(expensive.items = .N), by = brand]
# Changing df without creating a copy of it - ":=" operator a.k.a "set" operator
# x[, new.columns := expr] / x[, `:=` list(col1 = expr1, col2 = expr2)] /
# x[, c("col1", "col2") := list(expr1,expr2)] / x[i, new.column := expr, by]
# setkey - setup a key in a relation
setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)
key(products)
# JOIN
merge(purchases, products, by = 'product_id')
# join using multiple attributes (columns)
# merge(purchases, products, by = 'product_id', 'attr2')
# if keys in relations are named differently: by.x, by.y:
merge(purchases, products, by.x = 'product_id', by.y = 'product_id')
# LEFT and RIGHT join : all.x, all.y
merge(purchases, products, all.x = T, all.y = F) # LEFT JOIN
# join with []
purchases[products, on = 'product_id']
# J, SJ, CJ - search in data.table, data.table output, J (join) - constructs a table by key, 
# SJ (soft join) works out of [] and works faster, creates keys for columns
products[J(c(158,208))] #searching by key
(SJ(c(158,208)))
key(SJ(c(158,208)))
# CJ - cartesian multiplication, creates keys for all columns
(CJ(c(158,208), c("Supra", "Func"))) 
key(CJ(c(158,208), c("Supra", "Func"))) 
purchases.with.brands <- merge(
  purchases,
  products[, .(product_id, brand)],
  by = 'product_id'
)
pop.20.brands <- head(
  purchases.with.brands[, .(total.brand.users = uniqueN(externalsessionid)), by = brand][order(-total.brand.users)], 20)
pop.20.brands
# Homework 4-5
# Homework 4
product.category <- fread('product-categories.csv')

get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  df <- merge(purchases, product.category, by = 'product_id');
  df[order(category_id), .(totalcents = sum(totalcents), quantity = sum(quantity)), by = category_id]
}

get.category.ratings(purchases, product.category)
# Homework 5
mark.position.portion <- function(purchases) {
  df <- purchases[quantity > 0]
  df[, "portion" := sum(totalcents), by = ordernumber]
  df[, "price.portion" := sprintf("%.2f", round(100*totalcents/portion, 2))][order(ordernumber), .(totalcents, ordernumber, quantity, product_id, price.portion)]
}
View(mark.position.portion(purchases))
