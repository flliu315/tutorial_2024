# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This scribes how to use R packages and 
#          how to write effective R code.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-05
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

####################################################
# 01-basic R function
####################################################
# A: elementary calculation
10-(2+3) 
(3*8)/(2*3)
log(12)
sqrt(121)

# replicate something many times

rep("words",4)
rep(x=1, times=5)

# make a sequence of numbers

seq(1,5,1)
seq(from=100, to=10, by= -10)
rep(seq(1,3,1),times=2)

################################################
# B: various operators
# https://www.datamentor.io/r-programming/operator
# Assignment Operators
x <- 5
x
x <- 9
x
10 -> x
x

x <- 10-(2+3) 
x

# Operation on Vectors
x <- c(2, 8, 3)
y <- c(6, 4, 1)
x + y
x > y
# a mismatch in length
x <- c(2, 1, 8, 3)
y <- c(9, 4)
x + y
x - 1
x + c(1, 2, 3)

# Arithmetic Operators
x <- 5
y <- 16
x + y
x - y
x * y
y / x
y ^ x
y %/% x # Integer Division (floor)
y %% x # Remainder from division

floor(8.9)
ceiling(8.9)
round(8.4)
round(8.5)
round(8.6)

# Relational Operators
x <- 5
y <- 16
x < y
x > y
x <= 5
y >= 20
y == 16
x != 5

# Logical Operators

# OR operator (|)
x <- c(TRUE, TRUE, FALSE, FALSE)
y <- c(TRUE, FALSE, TRUE, FALSE)
!x
x | y

# AND operator (&)
x <- c(TRUE, TRUE, FALSE, FALSE)
y <- c(TRUE, FALSE, TRUE, FALSE)
x & y

# Short-circuit OR operator (||)
x <- c(TRUE, FALSE, FALSE, FALSE)
y <- c(FALSE, TRUE, FALSE, FALSE)
x || y

# Short-circuit AND operator (&&)
x <- c(FALSE, TRUE, TRUE, TRUE)
y <- c(TRUE, FALSE, TRUE, TRUE)
x && y

x <- c(TRUE, FALSE, 0, 6)
y <- c(FALSE, TRUE, FALSE, TRUE)
!x
x & y
x && y
x | y
x || y

###################################################
# C: data visualization
?plot.function

x <- c(1,2,3,4,5)
y<- c(1,4,9,16,25)
plot(x, y)

# shape of the markers (pch)
# pch = 0-25, 0 is for a square, 1 is for a circle, 
# 3 is for a triangle, 4 is for a cross and so on.
plot(x, y, 
     pch =4)

# Size of the markers (cex)
plot(x, y, pch =0, 
     cex = 0.5)

# Color of the markers (col)
colors()
plot(x, y, pch =0, cex = 1.5, 
     col="black")

# Connecting points with lines (type)
# type = "l", "o", "p", "b"
plot(x, y, pch =0, cex = 1.5, col="black", 
     type="b")

# Varying the lines (lty, lwd)
plot(x, y, pch =0, cex = 1.5, col="red", type="l",
     lwd=2,lty=1)

# Adding graphic information
plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1,
     main="This is a graph",col.main='blue') # main and its col

plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1, 
     main="This is a graph",col.main='blue',
     xlab="temperature",ylab="body size") # x and y labels

plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1, 
     main="This is a graph",col.main='blue',
     xlab="temperature",ylab="body size") 

legend('topleft',inset=0.05,"body size",lty=3,col='green',lwd=4)

# Overlaying Graphs
x=seq(2,10,0.1)
y1=x^2
y2=x^3
plot(x,y1,type='l',col='red')
lines(x,y2,col='green')
legend('bottomright',inset=0.05,c("Squares","Cubes"),lty=1,col=c("red","green"),title="Graph type")

# Adding Lines to a Plot
x=seq(2,10,0.1)
y1=x^2
y2=x^3
plot(x,y1,type='l',col='red')
lines(x,y2,col='green')
legend('bottomright',inset=0.05,c("Squares","Cubes"),lty=1,col=c("red","green"),title="Graph type")

abline(a=4,b=5,col='blue') # a= slope and b=intercept
abline(h=c(4,6,8),col="dark green",lty=2)
abline(v=c(4,6,8),col="dark green",lty=2)

##################################################
# 02-user libraries
##################################################

# finding and selecting packages
install.packages("packagefinder", dependencies = TRUE)
library(packagefinder)
findPackage("community ecology") 

# A: from CRAN
# Install packages by IDE or using install.packages()

install.packages('readr')
install.packages(c('readr', 'ggplot2', 'tidyr'))

# B: from GitHub
install.packages('devtools')
devtools::install_github('rstudio/shiny')

# C: from special Repositories
install.packages('furrr',
    repos='http://cran.us.r-project.org',
    dependencies=TRUE)

# D: from Zip files
# Installing R Packages from Zip Files by IDE or
install.packages('C:/Users/User/Downloads/abc_2.1.zip',
repos=NULL, type='source')


# using pak to install R packages

# first install pak package
install.packages("pak")
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

# Install packages from CRAN or Bioconductor
pak::pkg_install("clusterProfiler")

# Install packages from github
pak::pkg_install("tomwenseleers/export")

# help yourself about using packages

# Helping yourself
help(package="tidyverse")

# Vignettes Demonstrations
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")

# Searching for Help
apropos("^tidyverse")
ls("package:tidyverse")
help.search("^tidyverse")

##################################################
# 03-using self-defined functions
##################################################

#Creating a function without an argument
new.function <- function(){
  for(i in 1:5){
    print(i^2)
  }
}

new.function()

# Creating a function with argments
new.function <- function(a){
  for(i in 1:a){
    b <- i^2
    print(b)
  }
} 
# Calling the function supplying arguments
  new.function(10)
  
source("scripts/coldiss.R")
  
########################################
# 04-gptstudio for Rstudio
########################################
install.packages("pak")
pak::pak("usethis")
pak::pak("MichelNivard/gptstudio")
usethis::edit_r_environ()
Sys.getenv("OPENAI_API_KEY")
gptstudio:::gptstudio_chat()