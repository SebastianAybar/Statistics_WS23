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

#Aufgabe 5
#(a) Construct the 95% confidence upper bound for µ.
n <- 51
alpha <- 0.05
mw <- 300
s <- 60

mw+qt(1-alpha, df=n-1)s/sqrt(n)

#(b) Construct the 95% confidence lower bound for σ.
lb <- (n-1)s^2/qchisq(1-alpha, n-1)

#6. At a certain farm the weight of a peach (in ounces) at harvest time
#is a normally distributed random variable with standard deviation 0.5.
#How many peaches must be sampled to estimate the mean weight with
#a margin of error ±0.2 and with 95% confidence.

alpha <- 0.05
margin <- 0.2
s <- 0.5

ceiling((qnorm(1-alpha/2)s/margin)^2)


#7. You read about a survey in a newspaper and find that 70% of the 250
#people sampled prefer candidate A.
#####Confidence Interval for an unknown proportion#####

n <- 250
p.hat <- 0.7
alpha <- 0.05
q <- qnorm(1-alpha/2)
x <- 2500.7

#approximation
p.hat-qsqrt(p.hat(1-p.hat)/n)
p.hat+qsqrt(p.hat(1-p.hat)/n)

#exact values
binom.test(x, n, alternative = "two.sided", conf.level = 1-alpha)$conf.int

#Aufgabe 8
n <- 100
alpha <- 0.05
p.hat <- 0.4
q <- qnorm(1-alpha)
x <- n * p.hat

ub_appr <- p.hat + q * sqrt(p.hat * (1-p.hat) / n)
ub_exact <- binom.test(x, n, alternative = "less", conf.level = 1-alpha)

#Aufgabe 10
alpha <- 0.01
n <- 10
qt(1-alpha/2, df = n-1)

#Aufgabe 11
sigma <- 1.8
n <- 10
q <- pnorm(2.81)














