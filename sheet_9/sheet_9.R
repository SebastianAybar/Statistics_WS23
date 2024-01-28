#SHEET 9
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1
x <- c(5.46, 5.34, 4.34, 4.82, 4.4, 5.12, 5.69, 5.53, 4.77, 5.82)
y <- c(5.45, 5.31, 4.11, 4.69, 4.18, 5.05, 5.72, 5.54, 4.62, 5.89, 5.6, 5.19,
       3.31, 4.43, 5.3, 4.09)

alpha <- 0.05
sd_x <- 0.5
sd_y <- 0.6
mean_x <- mean(x)
mean_y <- mean(y)
lenght_x <- length(x)
length_y <- length(y)

t <- (mean_x - mean_y) / sqrt((sd_x/lenght_x) + (sd_y/length_y))
t < qnorm(alpha)
#These annehmen



#Aufgabe 2
x <- c(7.06, 11.84, 9.28, 7.92, 13.5, 3.98, 3.82, 7.34, 8.7, 9.24, 4.86, 3.32,
       12.78, 12, 5.24, 11.4, 6.56, 9.04, 7.72, 9.26, 7.88, 8.6, 9.3, 8.42, 8.54)

y <- c(8.68, 6, 6.3, 10.24, 10.88, 5.36, 7.82, 4.7, 9.02, 9.78, 6.9, 5.8, 13.56,
       10.32, 13.3, 11.38, 7.94, 10.74, 13.68, 14.92, 7.42, 10.36, 10.54,
       5.22, 13.74, 12.98, 10.34, 10.02, 17.8, 13.04, 5.2, 9.4, 11.18, 12.68,
       12.36)

n <- length(x)
m <- length(y)
sigma_sample_x <- sd(x)
sigma_sample_y <- sd(y)
mean_sample_x <- mean(x)
mean_sample_y <- mean(y)
var_x <- var(x)
var_y <- var(y)
alpha <- 0.05

T <- ((var_x/n) + (var_y/m))^2 / (((var_x/n)^2 / (n-1)) + ((var_y/m)^2 / (m-1)))
t <- t.test(x,y, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)
#H0 = Mu_Y < Mu_X
T < t$statistic
#annehmen


#Aufgabe 3
# H_null = Mu_alc > Mu_
water <- c(16, 15, 11, 20, 19, 14, 13, 15, 14, 16)
alcohol <- c(13, 13, 10, 18, 17, 11, 10, 15, 11, 16)
alpha <- 0.05
n_water <- length(water)
n_alcohol <- length(alcohol)
var_water <- var(water)
var_alcohol <- var(alcohol)

T <- ((var_water/n_water) + (var_alcohol/n_alcohol))^2 / (((var_water/n_water)^2 / (n_water-1)) + ((var_alcohol/n_alcohol)^2 / (n_alcohol-1)))
t <- qnorm(1-alpha)
T > t$statistic
#ablehnen







