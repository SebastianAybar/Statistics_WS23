library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 3.4.
sample <- c(247.4, 249.0, 248.5, 247.5, 250.6, 252.2, 253.4, 248.3, 251.4, 246.9,
            249.8, 250.6, 252.7, 250.6, 250.6, 252.5, 249.4, 250.6, 247.0, 249.4)
#(a) for the mean, if the standard deviation is known
n <- length(sample)
alpha <- 0.05
sigma <- 2
mu_X <- mean(sample)
sigma_X <- sigma/n^0.5
q <- qnorm(1-alpha/2)
u <- mu_X - q * sigma_X
o <- mu_X + q * sigma_X

#(b) for the mean, if the standard deviation is unknown
n <- length(sample)
alpha <- 0.05
sigma <- sd(sample)
mu_X <- mean(sample)
sigma_X <- sigma/n^0.5
q <- qt(1-alpha/2, df = n-1)
u <- mu_X - q * sigma_X
o <- mu_X + q * sigma_X

#(c) for the variance, if the mean is known
n <- length(sample)
mu <- 250
alpha <- 0.05
Qn <- sum((sample - mu)^2)
qchi_1 <- qchisq(1-alpha/2, n = n)
qchi_2 <- qchisq(alpha/2, n = n)
u <- Qn / qchi_1
o <- Qn / qchi_2

#(d) for the variance, if the mean is unknown
n <- length(sample)
sigma <- sd(sample)
alpha <- 0.05
qchi_1 <- qchisq(1-alpha/2, df = n-1)
qchi_2 <- qchisq(alpha/2, df = n-1)
u <- (n-1) * sigma^2 / qchi_1
o <- (n-1) * sigma^2 / qchi_2







