#SHEET ADDITIONAL MATERIALS
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("TeachingDemos")

#Inferential Statistics

#1. Consider the sample
#These are values of independent normally distributed random variables
#with sigma = 0.2.

#(a) Determine a confidence interval of the expected value at the 99% level.
x <- c(0.92,0.83,0.81,0.70,0.88,0.73,1.05,0.91,0.83,0.67,0.94,0.90,0.91,
       0.83,0.84,0.96,0.87,0.91,0.98,0.84,0.88,0.76,0.99,0.89,0.82)
n <- length(x)
mw <- mean(x)
sd <- sd(x)
alpha <- 0.01
sigma <- 0.2

ub <- mw+qnorm(1-alpha/2)sigma/sqrt(n)
lb <- mw-qnorm(1-alpha/2)sigma/sqrt(n)
ub; lb
#Was ist der expected Value?
#(b) Find the length of the confidence interval.
ub - lb
#The lenght of the Confidence Interval is 

#(c) The length of the confidence interval should be 0.15. Find
#i. how many sample values are necessary.
#ii. an appropriate confidence level


#(d) Assume now that the population variance is unknown. Conduct a
#suitable statistical test at a 5% level, to check whether the mean
#is greater 0.9.
h0: mu <= 0.9
h1: mu > 0.9
alpha <- 0.05
mw <- mean(x)
sd <- sd(x)
p0 <- 0.9
n <- length(x)

#Exact Value possible, sample is given
t.test(x, mu = 0.9, alternative = "greater", conf.level = 1-alpha)
#FRAGEN Ist bei einseitigen Hypothesen Tests immer der Parameter (in unserem Fall mean 0.9) gegeben?
#p-value < alpha = reject
#0.9647 < 0.05 FALSE = accept

#Approximierter Wert
test.stat <- ((mw-p0)/sd)*sqrt(n) #-1.892788
Q <- qt(1- alpha, df = n-1) #1.710882
#Annehmen, weil  1.710882 - unendlich, -1.892788 nicht im Interval deswegen accept



