#SHEET 9
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("TeachingDemos")


###################################################################
#Var.test:
#WENN p-value < alpha dann WELSH TEST
#WENN p-value > alpha dann T.TEST

#t.test:
#p-value < alpha h0 reject 
#p-value > alpha h0 accept

#H0 Hypothesen: >= <= =
#H1 Hypothesen: > < !=
###################################################################

#Aufgabe 1
x <- c(5.46, 5.34, 4.34, 4.82, 4.4, 5.12, 5.69, 5.53, 4.77, 5.82)
y <- c(5.45, 5.31, 4.11, 4.69, 4.18, 5.05, 5.72, 5.54, 4.62, 5.89, 5.6, 5.19,
       3.31, 4.43, 5.3, 4.09)

alpha <- 0.05
sd_x <- 0.5
sd_y <- 0.6
var_x <- sd_x^2
var_y <- sd_y^2
mean_x <- mean(x)
mean_y <- mean(y)
lenght_x <- length(x)
length_y <- length(y)

T <- (mean_x - mean_y) / sqrt((var_x/lenght_x) + (var_y/length_y))
t <- qnorm(alpha)
#H0 = Mu1 >= Mu2
#H1 = Mu1 < Mu2
T < t
#annehmen


#Aufgabe 2
#H0 = Mu_x >= Mu_y
#H1 = Mu_x < Mu_y
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

#ist hier nicht notwendig, t.test reicht
T <- ((var_x/n) + (var_y/m))^2 / (((var_x/n)^2 / (n-1)) + ((var_y/m)^2 / (m-1)))
#Case 2 - t.test
t.test(x,y, alternative = "less", paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)
#Case 3 - welsh test
t.test(x,y, alternative = "less", paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)

#F-Test
var.test(x, y, alternative = "less", paired = FALSE, conf.level = 1-alpha)
#WENN p-value < alpha, dann WELSH TEST
#WENN p-value > alpha, dann T.TEST


#Aufgabe 3
#H0 = Mu_water = Mu_alcohol
#H1 = Mu_water != Mu-alcohol
water <- c(16, 15, 11, 20, 19, 14, 13, 15, 14, 16)
alcohol <- c(13, 13, 10, 18, 17, 11, 10, 15, 11, 16)
alpha <- 0.05
#Wenn wir einen "paired" Datensatz haben, ist Welsh ausgeschlossen
t.test(water, alcohol, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 1-alpha)
#p-value < alpha , H0 rejected, risk Type 1 Error







