#SHEET 8
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("TeachingDemos")

#Aufgabe 4
#H0 = p<= 0.8
#H1 = p > 0.8
#Approximation
alpha <- 0.05
test.stat <- 0.06/sqrt((0.8*0.2)/200)
qnorm(1-alpha) # [1.644854, +unendelich]
#test.stat liegt im Intervall und daher H0 ablehnen, risk Type 1 Error

#Exact Value with R
binom.test(172, p = 0.8, n = 200, conf.level = 1-alpha, alternative = "greater")
#Wenn p-Value < alpha, dann rejecte ich H0
#Wenn p-Value > alpha, dann H0 annehmen
#Hier rejecten wir H0, risk Type 1 Error

#6. A bag of potato chips of a certain brand has an advertised weight of 250
#grams. Actually, the weight (in grams) is a random variable. Suppose
#that a sample of 81 bags has mean 248 and standard deviation 5. At
#the 0.05 significance level, conduct the following tests and calculate the
#p-values.

#(a) H0 : μ ≥ μ0 
#    H1 : μ < μ0

#Approximation
n <- 81
alpha <- 0.05
mw <- 248
sd <- 5
mu <- 250

test.stat <- ((mw-mu)/sd)*sqrt(n) #-3.6
Q <- -qt(1-alpha, df = n-1) #-1.6
-unendlich <= -3.6 <= -1.6 #ist im rejection Region -> rejecten







