library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("TeachingDemos")

#Probability
#Aufgabe 1a
1 - pbinom(39, size = 100, prob = 0.5) #0.9823999

#Aufgabe 1b
#per Hand
(factorial(100) / (factorial(30) * factorial(50) * factorial(20))) * 0.4^30 * 0.5^50 * 0.1^20 #4.869048e-05
#R
pmultinom()

#1c

