#sheet 6
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1.1.a
#Consider an urn with 100 balls, where are 30 balls of them are red. 
#20 balls are randomly drawn and let X be the number of red drawn balls.
total_balls <- 100
red_balls <- 30
drawn_balls <- 20
possible_X <- 0:drawn_balls
probs <- dhyper(x = possible_X, m = red_balls, n = total_balls - red_balls, k = drawn_balls)
barplot(probs, 
        names.arg = possible_X, 
        xlab = "Number of Red Balls Drawn", 
        ylab = "Probability", 
        main = "Hypergeometric Distribution")
print(probs)
