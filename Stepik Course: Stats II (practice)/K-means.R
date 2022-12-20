library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw()
