library(tidyverse)
df <- diamonds
# note how dots are grouped by color, but smoothing is performed on the whole dataset (without grouping)
ggplot(diamonds, aes(price, carat))+
  geom_point(aes(color = color), size = .1, shape = 3)+
  geom_smooth(color = "black", linetype = 2, size = 1)

dt <- airquality
glimpse(dt)
t <- summarise(group_by(dt, Month), mean_temp = mean(Temp),
                                    mean_wind = mean(Wind))
ggplot(t, aes(Month, mean_temp))+
  geom_line(linetype = 2)+
  geom_point(aes(size = mean_wind))+
  geom_hline(yintercept = 75, color = "red")

#errorbar() and pointrange()
#groupby
df <- group_by(mtcars, am, vs)
# find min/max values
# 1.96 is 2 sigmas in z-values
se_df <- summarise(df,
                   mean_mpg = mean(mpg),
                   y_max = mean(mpg) + 
                     1.96 * sd(mpg) / sqrt(length(mpg)),
                   y_min = mean(mpg) - 
                     1.96 * sd(mpg) / sqrt(length(mpg)))
ggplot(se_df, aes(x = factor(am), y = mean_mpg, col = factor(vs), group = factor(vs)))+
  geom_errorbar(aes(ymax = y_max, ymin = y_min), width = .2)+
  geom_point(size = 2, shape = 21, fill = 'white')+
  geom_line(linewidth = .1)

ggplot(se_df, aes(x = factor(am), y = mean_mpg, col = factor(vs), group = factor(vs)))+
  geom_pointrange(aes(ymax = y_max, ymin = y_min), size = .8, shape = 21, fill = 'white', color = 'red')+
  geom_line()

# ggplot2's stat_summary() + using manually created functions, position dodge
ggplot(mtcars, aes(factor(am), mpg, col = factor(vs), group = factor(vs)))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = .2)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')

# requires c(y,ymin,ymax) output vector
sd_error <- function(x) {c(y = mean(x), ymin = mean(x)-sd(x), ymax = mean(x)+sd(x))}
ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), group = factor(cyl)))+
  stat_summary(fun.data = sd_error, geom = 'errorbar', width = .2, position = position_dodge(.2))+
  stat_summary(fun.data = sd_error, geom = 'point', position = position_dodge(.2))+
  stat_summary(fun = mean, geom = 'line', position = position_dodge(.2))

# homework 1-4
ggplot(mtcars, aes(factor(am), mpg))+
  geom_violin()+
  geom_boxplot(width = 0.2)

df <- read.csv('https://stepic.org/media/attachments/course/724/sales.csv', 
               encoding = 'UTF-8',
               stringsAsFactors = T)  
spec(df)
glimpse(df)
summary(df)
str(df)
ggplot(df, aes(income, sale))+
  geom_point(aes(color = shop))+
  geom_smooth(method = 'lm')

ggplot(df, aes(shop, income, col = season))+
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', position = position_dodge(.2))

ggplot(df, aes(date, sale, col = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', position = position_dodge(.2), width = 1)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')
# facet_grid(), facet_wrap()
# facet grid syntax: (rows ~ columns)
ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_grid(color ~ cut)
# 1 faceting variable
ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_grid(cut~.)
# margins - add TOTAL columns & rows in a facet_grid
mtcars <- mutate(mtcars, am = factor(am, labels = c("A", "M")),
                 vs = factor(vs, labels = c("V", "S")))
ggplot(mtcars, aes(hp,mpg))+
  geom_point(aes(col = cyl))+
  facet_grid (am~vs, margins = T)+
  geom_smooth(method = 'lm')

#facet wrap
# 2 faceting features
ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_wrap(~ color + cut, nrow = 5)
# 1 feature
ggplot(diamonds, aes(carat))+
  geom_density()+
  facet_wrap(~ cut, ncol = 1)

ggplot(diamonds, aes(carat, price))+
  geom_smooth(color = "black")+
  facet_wrap(~ cut, ncol = 2)

#homework 1-4
ggplot(mtcars, aes(mpg))+
  geom_dotplot()+
  facet_grid (am~vs) 

ggplot(iris, aes(Sepal.Length))+
  geom_density()+
  facet_wrap(~ Species)

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ Species)

df <- read.csv('https://stepik.org/media/attachments/course/724/myMovieData.csv', stringsAsFactors = T)
  ggplot(df, aes(Type, Budget))+
    geom_boxplot()+
    facet_grid(. ~ Year)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ggplot2 Scale

#scale_*_continuous/discrete
ggplot(mtcars, aes(mpg, hp, col = factor(am)))+
  geom_point(shape = 3)+
  scale_x_continuous(name = 'mpg	Miles/(US) gallon', breaks = seq(min(mtcars$mpg), max(mtcars$mpg), by = 10),
                     limits = c(1,40),
                     expand = c(1,1))+
  scale_y_continuous(limits = c(1,400))+
  scale_color_manual(values =c("Blue","Orange"),
                     name = "Gearbox",
                     labels = c("Auto", "Manual"))
  
 #scale shortcuts:
ggplot(mtcars, aes(mpg, hp, col = factor(am)))+
  geom_point()+
  xlab('MILES PER GALLON')+
  ylab('HORSPOWER')+
  xlim(1,35)
 
ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density()+
  facet_grid(. ~ am)+
  scale_fill_manual(values = c('red', 'green'))+
  scale_fill_identity()

ggplot(mtcars, aes(hp, mpg, size = disp, shape = factor(vs)))+
  geom_point(color = "blue")+
  scale_size_continuous(name = "DISP", breaks = seq(100, 400, 40))+
  scale_shape_discrete(name = "VS", solid = F)

#homework 1
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, col = Species))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_continuous(name = "Длина чашелистика", breaks = seq(4,8,1), limit = c(4,8))+
  scale_y_continuous(name = "Длина лепестка", breaks = seq(1,7,1), limit = c(1,7))+
  scale_color_discrete(name = 'Вид цветка',
                       labels = c('Ирис щетинистый','Ирис разноцветный','Ирис виргинский'))

# ggplot Theme and overall design
ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl)))+
  geom_boxplot()+
  #colorbrewer2.org
  scale_fill_brewer(type = 'qual', palette = 3)+
  theme_bw()

ggplot(mtcars, aes(mpg, hp, col = factor(cyl)))+
  geom_point(size = .9)+
  scale_color_brewer(type = "qual", palette = 6)+
  theme(text = element_text(size = 14), 
        axis.line.x = element_line(size = 2),
        axis.line.y = element_line(size = 2))
#ggthemes
library(ggthemes)
ggplot(mtcars, aes(mpg, hp, col = factor(cyl)))+
  geom_point(size = .9)+
  theme_stata()

#practice notes
d <- read.csv('https://stepic.org/media/attachments/course/724/example_data.csv')
p <- ggplot(d, aes(date, percent, col = system, group = system))+
  geom_line()+
  geom_vline(xintercept = 7.5, linetype = 2, color = 'white')+
  geom_point(shape = 21, size = 5, fill = 'black')+
  geom_point(shape = 21, size = 4.5)+
  scale_y_continuous(breaks = c(0, .04, .08, .11, .15),
                     limit = c(0, .15),
                     labels = scales::percent)+
  scale_color_manual(values = c('orangered1',
                                'red',
                                'cyan',
                                'yellow1',
                                'springgreen2'))+
  xlab("")+
  ylab("")+
  ggtitle("TOP 5 Linux distributions (% of total per year)")+
  theme_classic()

#saving DIY plot theme
my_theme_black <-  theme(legend.title = element_blank(),
          legend.position = "top",
          plot.background = element_rect(color = 'black', fill = 'black'),
          panel.background = element_rect(color = 'black', fill = 'black'),
          legend.background = element_rect(fill = 'black'), 
          text = element_text(color = 'white'),
          panel.grid.major.y = element_line(color = 'gray50', linetype = 'longdash'),
          axis.text.x = element_text(face = 'bold', size = 9),
          axis.text.y = element_text(face = 'bold', size = 9),
          legend.text = element_text(size = 9, color = 'white'))
# using your own theme with ggplot
p + my_theme_black

# grid library to add a watermark
library(grid)
grid.text("Taken from DA in R II course", x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontface = "bold", fontsize = 9, col = 'white'))
# plotly - interactive graphics!
# plot -> web page (Plots -> Viewer)
library(plotly)
interactive_p <- ggplotly(p+my_theme_black)
interactive_p

# mesh3d & surface - drawing 3D objects!
mesh <- data.table(
  x = rnorm(40),
  y = rnorm(40),
  z = rnorm(40)
)
plot_ly(mesh, type="mesh3d", x = ~x, y = ~y, z = ~z, alphahull = 0)

# instead of triangulation - i,j,k indices
points <- data.table(
  x = c(0.2, 0.8, 0, 1),
  y = c(0, 0, 1, 1),
  z = c(0, 0, 0, 0)
)
i.s <- c(0, 2, 1)
j.s <- c(1, 1, 2)
k.s <- c(2, 3,  3)
plot_ly(points, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")

# Homework 1 - drawing 3D teapot
xyz <- read_csv2('https://stepic.org/media/attachments/course/724/teapot.csv')

make.fancy.teapot <- function(xyz) {
  plot_ly(xyz, x = ~x, y = ~y, z = ~z, i = ~seq(0, nrow(xyz)-1, by = 3), 
          j = ~seq(1, nrow(xyz)-1, by = 3), k = ~seq(2, nrow(xyz)-1, by = 3),
          type = "mesh3d"
          )
}
make.fancy.teapot(xyz)