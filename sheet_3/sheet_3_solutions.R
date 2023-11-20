#Aufgabe 2.5.
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

#Beobachtungen
#a) Die Abweichungen zwischen der empirischen Verteilungsfunktion F und der Funktion H nehmen ab. 
#Für große Stichprobengrößen sind beide Funktionen mehr oder weniger identisch.
#b) Die realisierbaren Werte der Quantile vom Typ 1 sind {1,2,...20}, während
#Quantile des Typs 7 können jeden Wert aus dem Intervall [1,20] annehmen.
#c) Quantile vom Typ 1 sind für diskrete Variablen geeignet. Im Falle von kontinuierlichen 
#Variablen können beide Typen verwendet werden und insbesondere bei großen Stichprobengrößen sind die
#Werte mehr oder weniger identisch sind.

#Aufgabe 2.6.
data1 <- tibble(
  type = c(rep("non-player",10), rep("beginner",10),rep("tournament",10)),
  res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
          32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,
          40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))

# alternative: tidy the messy dataset data
data1 %>% 
  as_tibble() %>%
  gather(key = "type", value = "res")

measures <- data1 %>%
  group_by(type) %>%
  summarise(Min = min(res),Max=max(res),
            q1=quantile(res,0.25,type=1),q2=quantile(res,0.5,type=1),
            q3=quantile(res,0.75,type=1),
            Mean=mean(res),variance=var(res),
            interquartile_range=q3-q1)
measures

# Boxplots
boxplot(data1[,1],data1[,2],data1[,3], 
        names=colnames(data1),
        main = "side by side boxplots",
        xlab = "player type", ylab = "rem. chess positions")

# Boxplots with ggplot
boxplot(res ~ type, data = data1)
# solution with ggplot()
# changing the order in the side by side boxplots by adding a factor to type
data$type <- factor(data$type, levels = c("non-player", "beginner","tournament"))
ggplot(data = data1) + 
  geom_boxplot(mapping = aes(x=type, y=res, group = type)) +
  geom_point(mapping = aes(x=type,y=res,group=type)) +
  xlab("player type") +
  ylab("rem. chess positions") +
  ggtitle("side by side boxplots with marked values") +
  theme_bw()

