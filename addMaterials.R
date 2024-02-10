#SHEET ADDITIONAL MATERIALS
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")
library("TeachingDemos")

######### Descriptive ###############
#1
corona <- read_csv("C:/Users/sebas/OneDrive/Dokumente/GitHub/Statistics_WS23/additionalMaterials/corona.csv")
corona_tibble <- corona %>% as.tibble()
#2
# continent, country: qualitative, nominal
# year, day, month: quantitative, interval
# der rest: quantitative, ratio
#3
corona %>% filter(month == 10 & location == "Germany") %>% 
  select(day, new_cases, new_deaths)
#4
# Germany, Spain, Afghanistan
corona %>% filter(location %in% c("Germany,","Spain","Afghanistan")) %>%
  group_by(location, month) %>%
  summarise(new_cases_summonth = sum(new_cases), new_deaths_summonth = sum(new_deaths))
#5
#eigentliche Lösung
corona %>% filter(location %in% c("Germany,","Spain","Afghanistan")) %>%
  group_by(month) %>%
  mutate(
    total_new_cases = sum(new_cases),
    total_new_deaths = sum(new_deaths)
  ) %>%
  summarise(Max.cases = max(total_new_cases),
            Mean.cases = mean(total_new_cases),
            Median.cases = median(total_new_cases),
            Min.cases = min(total_new_cases),
            Max.deaths = max(total_new_deaths),
            Mean.deaths = mean(total_new_deaths),
            Median.deaths = median(total_new_deaths),
            Min.deaths = min(total_new_deaths))

#wortwörtliche Lösung
summary <- corona %>%
  group_by(month) %>%
  summarize(min_cases = min(new_cases, na.rm = TRUE),
            min_death = min(new_deaths, na.rm = TRUE),
            max_cases = max(new_cases, na.rm = TRUE),
            max_death = max(new_deaths, na.rm = TRUE),
            mean_cases = mean(new_cases, na.rm = TRUE),
            mean_death = mean(new_deaths, na.rm = TRUE),
            median_cases = median(new_cases, na.rm = TRUE),
            median_death = median(new_deaths, na.rm = TRUE))
#7
covger <- corona %>% filter(location == "Germany") %>%
  select(day, new_cases, new_deaths)
covger
#a)
lm(covger$new_deaths~covger$new_cases)
# Parameter a = 17.53652
# Parameter b = 0.01213  
#b)
plot(covger$new_cases, covger$new_deaths, main = "Tode in Relation zu Tagen in DE", xlab = "ASDAD")
abline(a = 17.53652, b = 0.01213, col = "red")
#c)
cor(covger$new_cases, covger$new_deaths)
cor(covger$new_cases, covger$new_deaths)^2 # Coefficient of determination
#d)
17.53652+0.01213*20000
#e)
#a = steigung
#b = y abschnitt
#8)


########## Probability ###############
#Aufgabe 1
#frid = 0.4
#sa =0.5
# son = 0.1
#X~B(n = 100, p = 0.5)
#P(X >= 40)
#a)
1-pbinom(39, size = 100, prob = 0.5)
#0.9823999 
#b)
#multinomial distribution
#F = 30, S=50, Son = 20
factorial(100)*(0.4)^(30)*(0.5)^(50)*(0.1)^(20)/
  (factorial(30)*factorial(50)*factorial(20))
#c)
0.9^19*0.10
#d)
#n = 80
# P(<= 80) = 0.90
dtib <- tibble(
  n = 100:150,
  prob = pbinom(80,n,prob = 0.50)
)
dtib
view(dtib)

pbinom(80,size = 145, prob = 0.50)

#2
#X~N(mean = 3.075, var = 0.68)
#P(X>=300)
#X~N(mean = 3.075*100, var = 0.68*100) weil 100 Kunden
mu <- 0.2*3+0.4*4+0.25*2+0.15*2.5
var <- (3-mu)^2*0.2 + (4-mu)^2*0.4 + (2-mu)^2*0.25 + (2.5-mu)^2*0.15
1-pnorm(300, mean = mu*100, sd = sqrt(var*100))


#b)
# P(X<= UN) = 0.95
qnorm(0.95,mean = 3.075*100, sd =sqrt(0.68*100) )
pnorm(321.0638, mean = 3.075*100, sd =sqrt(0.68*100))

######### Inferential Statistics #############

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

#### ONE SAMPLE TEST, NORMALLY DISTRIBUTED, BOTH UNKNOWN, COMPARE Mu ==> Case 2 ####

# (d) Assume now that the population variance is unknown. Conduct a
#     suitable statistical test at a 5% level, to check whether the mean is greater 0.9.

#H0: mu <= 0.9
#H1: mu  > 0.9

alpha <- 0.05
mw <- mean(x)
sd <- sd(x)
p0 <- 0.9
n <- length(x)

### Exact Value possible, sample is given ###
t.test(x, mu = 0.9, alternative = "greater", conf.level = 1-alpha)
#p-value < alpha => reject

### Approximierter Wert ###
test.stat <- ((mw-p0)/sd)*sqrt(n) #-1.892788
Q <- qt(1- alpha, df = n-1) #1.710882
#Annehmen, weil  recejtion region [1.710882 ; +∞], -1.892788 nicht im Interval deswegen nicht rejection
#Wir riskieren einen Type 2 Error


(factorial(49)/factorial(6))/1000000


