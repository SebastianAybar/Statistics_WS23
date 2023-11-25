#sheet 4

library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1.1.
x <- c(2, 6, 3, 4, 5)
y <- c(3, 7, 4, 7, 6)
plot(x = x, y = y,
     xlim = c(0,10),
     ylim = c(0,10))

covariance <- cov(x,y)
print(covariance)
