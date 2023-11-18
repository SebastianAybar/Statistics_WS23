#sheet 3
library(tidyverse)

# a) Generate a random sample of size n from 1, 2, ..., 20
# and determine the empirical distribution function 
s.size <- 10
x <- sample(1:20, size = s.size, replace = TRUE)
emp.dist <- tibble(obs = x) %>%
  count(obs) %>%
  mutate(cum.rel.freq = cumsum(n)/sum(n)
  ) 

# b) Determine a R function to find quantile according
# to the definition given in the lecture
my.quantile <- function(x,p) {
  x_sort <- sort(x)
  return(x_sort[ceiling(length(x)*p)])
}

# c) Compare the results of your quantile function with
# the results of the R function
my.quantile(x, seq(0,1, by = 0.05))
quantile(x, probs = seq(0,1, by=0.05), type = 1)

# d) The R function quantile() evaluates quantiles of type 7
# if no type is specified. Type 7 quantile are defined by a linear 
# interpolation of the points 
# (0, x_(1)), (1/(n-1),x_(2)), (2/(n-1),x_(3)), ..., (n/(n-1),x_(n))
# with n = sample size. Visualize the evalution by a diagram 
# which contain these points, the linear interpolation and the
# R quantile of type 7 of order 0, 0.5, ..., 1
plot(y=sort(x), 
     x=(0:(s.size-1))/(s.size-1),
     type="b", 
     col = "black",
     ylim=c(0,22), 
     xlim=c(-0.1,1.1),
     xlab="p", 
     ylab="p quantile",
     main="quantiles type=7",
     sub = "black = linear interpolation, red = type 7 quantiles")

points(x=seq(0,1, by=0.05), y=quantile(x,prob=seq(0,1, by=0.05)), col="red")

# e) Create a table containing the quantiles of type 1 and type 7 of order
# 0,0.01, ..., 0.99,1. What are the possible values of the quantiles?
q1.q7 <- tibble(
  p = seq(0,1,by=0.01),
  q1 = quantile(x, probs=p,type=1),
  q7 = quantile(x, probs=p,type=7)
)
# feasible value type 1: all sample values
# feasible values type 7: all values in the interval 
# [min(sample values, max(sample values)]

# f) Create a diagram which visualize the empirical distribution function,
# the function H, which connects the points
# (0, x_(1)), (1/(n-1),x_(2)), (2/(n-1),x_(3)), ..., (n/(n-1),x_(n))
# with line segments (the above linear interpolation), type 1 and type 7
# quantiles and mention the difference between type 1 and type 7 quantiles.
plot(x=sort(x), y=(0:(s.size-1))/(s.size-1),
     type="b", col = "black",
     xlim=c(0,22), ylim=c(-0.1,1.1),
     ylab="p", xlab="x",
     main="Comparison of type 1 and 7",
     sub = "black = type 7, blue = type 1")

points(y=seq(0,1, by=0.05), x=quantile(x,prob=seq(0,1, by=0.05), type=7), col="black")
points(y=seq(0,1, by=0.05), x=quantile(x,prob=seq(0,1, by=0.05), type=1), col="blue")
lines(x=emp.dist$obs, y=emp.dist$cum.rel.freq, type="s",col="blue")

# g) Increase the sample size from 10 to 50 and then to 100 and create 
# the above diagram. What happens?
s.size <- 200
x <- sample(1:20, size = s.size, replace = TRUE)
emp.dist <- tibble(
  obs = x
) %>%
  count(obs) %>%
  mutate(
    cum.rel.freq = cumsum(n)/sum(n)
  ) 
plot(x=sort(x), y=(0:(s.size-1))/(s.size-1),
     type="l", col = "black",
     xlim=c(0,22), ylim=c(-0.1,1.1),
     ylab="p", xlab="x",
     main="Comparison of type 1 and 7, n=100",
     sub = "black = type 7, blue = type 1")
points(y=seq(0,1, by=0.02), x=quantile(x,prob=seq(0,1, by=0.02), type=7), col="black")
points(y=seq(0,1, by=0.02), x=quantile(x,prob=seq(0,1, by=0.02), type=1), col="blue")
lines(x=emp.dist$obs, y=emp.dist$cum.rel.freq, type="s",col="blue")

# Observations
# a) The deviations between the empirical distribution function F and the 
# function H decrease. For big sample sizes both function are more or less
# identical.
# b) The feasible values of type 1 quantiles are {1,2,...20} whereas
# quantiles of type 7 can take every value from the interval [1,20].
# c) Type 1 quantiles are suited for discrete variables. In case of continous 
# variables both types can be used and exspecially for large sample sizes the
# values are more or less identical.