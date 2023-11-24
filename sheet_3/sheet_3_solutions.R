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
data1$type <- factor(data1$type, levels = c("non-player", "beginner","tournament"))
ggplot(data = data1) + 
  geom_boxplot(mapping = aes(x=type, y=res, group = type)) +
  geom_point(mapping = aes(x=type,y=res,group=type)) +
  xlab("player type") +
  ylab("rem. chess positions") +
  ggtitle("side by side boxplots with marked values") +
  theme_bw()

#Aufgabe 2.7
# generate the data
distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)
# sorted data
sort_dist <- sort(distance)
sort_alt <- sort(altitude)

# mean and median
mean_dist <- mean(distance)
mean_alt <- mean(altitude)

# R offers several ways of calculating quantiles. Use type=1 
# to apply the method we have introduced.
quantiles_dist <- quantile(distance,probs = c(0.25,0.5,0.75),type=1)
quantiles_alt <- quantile(altitude,probs = c(0.25,0.5,0.75),type=1)

# interquartial range
interquartile_dist <- quantile(distance,probs=0.75,type=1)- quantile(distance,probs=0.25,type=1)
interquartile_alt <- quantile(altitude,probs=0.75,type=1)- quantile(altitude,probs=0.25,type=1)

# shape of the distributions
# distance: Q2 closer to Q3, i.e. distance might be left skewed
# altitude: Q2 closer to Q1, i.e. distance might be right skewed
boxplot(altitude, main = "altitude", xlab="altitude")
boxplot(distance, main = "distance", xlab="distance")
boxplot(altitude, distance, 
        main = "altitude and distance",
        names = c("distance", "altitude"))

# variance and standard deviation
var_dist <- var(distance)
var_alt <- var(altitude)
sd_dist <- sd(distance)
sd_alt <- sd(altitude)

# make the values comparable
distance.norm <- distance/mean(distance)
altitude.norm <- altitude/mean(altitude)
boxplot(altitude.norm, distance.norm, 
        main = "altitude and distance - normed",
        names = c("distance.norm", "altitude.norm"))

# coefficients of variation
sd(distance)/mean(distance) # = sd(distance.norm)
sd(altitude)/mean(altitude) # = sd(altitude.norm)



#Aufgabe2.8.
# inspect the description of the data set
?mpg()

# select only the variables displ and hwy and add 
# a colummn displ_class which denotes the belonging
# to one of the groups 
# low (1 < displ <= 3), medium (3 < displ <= 5),
# big (5 < displ <= 8)
tab <-
  mpg %>%
  select(displ,hwy) %>%
  mutate(displ_class = 
           cut(displ,breaks = c(1,3,5,8),
               labels = c("small","medium","big"))
  )
tab

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ.
stat_hwy_displ <-
  tab %>%
  group_by(displ) %>%
  summarise(mean=mean(hwy),
            min=min(hwy),
            max=max(hwy),
            q1=quantile(hwy,0.25, type=1),
            q2=quantile(hwy,0.5, type=1),
            q3=quantile(hwy,0.75, type=1),
            nobs = n())

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ_class.
stat_hwy_class <-
  tab %>%
  group_by(displ_class) %>%
  summarise(mean(hwy),
            min(hwy),
            max(hwy),
            q1=quantile(hwy,0.25, type=2),
            q2=quantile(hwy,0.5, type=2),
            q3=quantile(hwy,0.75, type=2),
            nobs = n()
  )
stat_hwy_class

# boxplots of hwy grouped by displ
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
# using ggplot
ggplot(data = tab) +
  geom_boxplot(mapping = aes(group = displ, x = displ, y = hwy)) + 
  theme_bw()

# boxplots of hwy grouped by displ_class
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
# using ggplot
ggplot(data = tab) +
  geom_boxplot(mapping = aes(group = displ_class, x = displ_class, y = hwy)) +
  #  geom_point(mapping = aes(group = displ_class, x = displ_class, y = hwy)) +
  theme_bw()

# both boxplots together
par(mfrow = c(1,2))
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
# zuruecksetzen von mfrow
par(mfrow = c(1,1))

