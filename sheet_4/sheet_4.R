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

#(a) covariance
covariance <- cov(x,y)
#(b)  coefficient of correlation
sd_x <- sd(x)
sd_y <- sd(y)
var_x <- var(x)
var_y <- var(y)
coef_cor <- covariance/(sd_x * sd_y)
#Funktion aus R
coef_cor <- cor(x,y)
#regression line: criterion variable Y and predictor variable X
linear_regression <- lm(y ~ x)
summary(linear_regression)
plot(x = x, y = y,
     xlim = c(0,10),
     ylim = c(0,10))
abline(linear_regression)
#regression line: criterion variable X and predictor variable Y
linear_regression <- lm(x ~ y)
summary(linear_regression)
plot(x = y, y = x,
     xlim = c(0,10),
     ylim = c(0,10))
abline(linear_regression)




