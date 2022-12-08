.libPaths()
installed.packages()

#open grid package from R library
library()
library(grid)
?require
grid.newpage()
grid.lines
#install package from CRAN repositories
install.packages('xts', dependencies = T)
#update packages
update.packages()
#sessioninfo tool
sessionInfo()
?sessionInfo
#Homework
install.packages('randtoolbox', dependencies = T)
library(randtoolbox)
help(package = "xts")
library(help = "quantmod")
