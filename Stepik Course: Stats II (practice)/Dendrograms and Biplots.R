# Building dendrograms and biplots
library(ggrepel)
x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()


d <- dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2)

library(ape)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
plot.phylo(tr) 
plot.phylo(tr, use.edge.length=FALSE)


library(FactoMineR)
df <- mtcars[,c(1,3:7)]
res.pca <- PCA(df, graph=TRUE)