#sheet 6
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




