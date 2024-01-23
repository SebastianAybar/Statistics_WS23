#SHEET 6
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1.1.(a)
#Consider an urn with 100 balls, where are 30 balls of them are red. 
#20 balls are randomly drawn and let X be the number of red drawn balls.
total_balls <- 100
red_balls <- 30
drawn_balls <- 20
possible_X <- 0:drawn_balls
probs <- dhyper(x = possible_X, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
#(b) Plot.
barplot(probs, 
        names.arg = possible_X, 
        xlab = "Number of Red Balls Drawn", 
        ylab = "Probability", 
        main = "Hypergeometric Distribution")
#Die Wahrscheinlichkeit ist für k=6 am höchsten!

#(c)Generate a sample of size 20 of values of X.
k_sample <- sample(possible_X, size = 20, replace = FALSE)
sample <- dhyper(k_sample, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
barplot(sample)

#(e)
q1 <- qhyper(0.25, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
q2 <- qhyper(0.50, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
q3 <- qhyper(0.75, m = red_balls, n = total_balls - red_balls, k = drawn_balls)

#
random_sample <- rhyper(20, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
print(random_sample)

#(d)
a <- phyper(15, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
b <- phyper(5-1, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
x <- a-b

#Augabe 2.1.

#dnorm()
x_values <- seq(-10, 10, .1)
density_values <- dnorm(x_values, sd = 1.5, mean = 2.5)
plot(x_values, density_values)
#pnorm()
prob_1 <- pnorm(2, sd = 1.5, mean = 2.5)
prob_2 <- 1 - pnorm(3.1, sd = 1.5, mean = 2.5)
prob_3 <- pnorm(3.5, sd = 1.5, mean = 2.5) - pnorm(1, sd = 1.5, mean = 2.5)
plot(x_values, pnorm(x_values, sd = 1.5, mean = 2.5), type = "l")
#qnorm()
x <- seq(0,1,.02)
y <- qnorm(x, sd = 1.5, mean = 2.5)
plot(x, y, type = "l")
plot(seq(0,1,.02), qnorm(seq(0,1,.02), sd = 1.5, mean = 2.5), type = "l")

#Beispiel pnorm und qnorm
quantil <- qnorm(0.975, mean = 0, sd = 1)
print(quantil)

wahrscheinlichkeit <- pnorm(1.96, mean = 0, sd = 1)
print(wahrscheinlichkeit)

plot(seq(-10, 10, 0.1), dnorm(seq(-10, 10, 0.1), mean = 0, sd = 1))

#rnorm()
stichprobe <-  rnorm(100, sd = 1.5, mean = 2.5)
hist(stichprobe)

#Aufgabe 2.2.
x_values <- seq(0, 20, .1)
density_values <- dnorm(x_values, sd = 0.6, mean = 12.8)
plot(x_values, density_values)
less_than_12_ounces <- pnorm(12, sd = 0.3439, mean = 12.8)
#die Standard Deviation müsste bei gerundet 0.3439 liegen 
#damit genau nur noch 1% der Potato Chips <= 12 ounces wiegen

#Um es ohne R zu machen brauchen wir dennoch den Wert des 1.Percentils
z_score <- qnorm(0.01) #mean und sd werden default auf 0 und 1 gesetzt
print(z_score)
#Z-Score Formel nach sd umgestellt
(12-12.8)/qnorm(0.01, mean = 0, sd = 1)


#Aufgabe 2.3.
#(a)
prob_at_least_750 <- 1 - pnorm(750, mean = 800, sd = 80)
prob_exceeds_1000 <- 1 - pnorm(1000, mean = 800, sd = 80)

#(b)
n <- 10000
mu <- 800
sigma <- 80
unteren_5 <- qnorm(0.025, mean = n*mu, sd = n^0.5*sigma)
oberen_5 <- qnorm(0.975, mean = n*mu, sd = n^0.5*sigma)
#Wenn ich 10 000 Fäden nebeneinander lege, haben diese mit einer 95% Wahrscheinlichkeit eine
#Gesamtlänge zwischen "unteren_5" und "oberen_5" m

#(c)
aktuell <- 1 - pnorm(750, mean = 800, sd = 80)

quantile <- qnorm(0.1, mean = 0, sd = 1)


#(d)
x <- pnorm(750, mean = 800, sd = 80)
print(x)

z <- rnorm(10, mean = 800, sd = 80)
print(z)

pbinom(q = 1, size = 10, prob = pnorm(750, mean = 800, sd = 80))


#Aufgabe 2.4.
#(b)
Peter <- pnorm(723, mean = 720, sd = 5) - pnorm(717, mean = 720, sd = 5)
Paul <- pnorm(723, mean = 722, sd = 2) - pnorm(717, mean = 722, sd = 2)
prob <- Peter * Paul

#Aufgabe 2.5.
#(a)
sample <- seq(0, 3, 0.1)
mu_melone <- 1.2
mu_pineapple <- 0.6
var_melone <- 0.3^2
var_pineapple <- 0.2^2
sigma <- (var_melone + var_pineapple)^0.5

density_values <- dnorm(sample, mean = mu_melone+mu_pineapple, sd = sigma)
plot(sample, density_values, type = "l")
#(b)
b <- pnorm(2, mean = mu_melone+mu_pineapple, sd = sigma)
#(c)
neues_mu <- 1.2 * 2 + 0.6 * 4
neue_var <- 2^2 * var_melone + 4^2 * var_pineapple
neues_sample <- seq(0, 9, 0.1)
neue_verteilung <- dnorm(neues_sample, mean = neues_mu, sd = neue_var^0.5)
plot(neues_sample, neue_verteilung)

#(d)
prob <- 1 - pnorm(4, mean = neues_mu, sd = neue_var^0.5)




