#SHEET 4
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1.1.
x <- c(2, 6, 3, 4, 5)
y <- c(3, 7, 4, 7, 6)
plot(x = x, y = y,
     xlim = c(0,10),
     ylim = c(0,10))

#(a) covariance
covariance <- cov(x,y)
#(b)  coefficient of correlation
sd_x <- sd(x)
sd_y <- sd(y)
var_x <- var(x)
var_y <- var(y)
coef_cor <- covariance/(sd_x * sd_y)
#Funktion aus R
coef_cor <- cor(x,y)
#regression line: criterion variable Y and predictor variable X
linear_regression <- lm(y ~ x)
summary(linear_regression)
plot(x = x, y = y,
     xlim = c(0,10),
     ylim = c(0,10))
abline(linear_regression)
#regression line: criterion variable X and predictor variable Y
linear_regression <- lm(x ~ y)
summary(linear_regression)
plot(x = y, y = x,
     xlim = c(0,10),
     ylim = c(0,10))
abline(linear_regression)

#Aufgabe 1.2.
x <- c(10, 9, 9, 11, 10, 10, 6, 10, 8, 12, 9, 4, 12)
y <- c(5, 5, 4, 6, 7, 5, 3, 4, 5, 7, 4, 2, 8)
#(a)
plot(x = x, 
     y = y,
     xlim = c(0,10),
     ylim = c(0,10))
#(b)
#positive
#(c)
cov <- cov(x = x, y = y)
cor <- cor(x = x, y = y)
#(d) y = ax + b
linear_regression <- lm(y ~ x)
plot(x = x, y = y,
     xlim = c(0,12),
     ylim = c(0,12))
abline(linear_regression)
#wir speichern uns die Koeffizienten der Regressionsgeradengleichung y = a + bx
#a ist die Steigung der Regressionsgeraden und b der Y-Achsenabschnitt
a <- linear_regression$coefficients[1]
b <- linear_regression$coefficients[2]

#(e)
y <- a + b * 8

#Tibble mit  den restlichen Werten und den fitted Values
data <- tibble(time_spend = x,
               exam_score = y, 
               residual_values = linear_regression$residuals,
               predicted_scores = linear_regression$fitted.values)

#(g)
#Bestimmtheitsmass/Determinationskoeffizient
B <- cor(results$time,results$score)^2

#Aufgabe 2.1.
#(b) no!
#(a)
data <- tibble(cafe = c(seq(1,5,1)),
               X = c(3,8,7,9,5),
               Y = c(6,7,10,8,4))

spearman_cor <- cor(data$X, data$Y, method = "spearman")
#(c)?

#Aufgabe 2.1.
#(a)
#(b)
values <- tibble(Pass = c(40,20,10),
               Fail = c(10,10,10))
#compute expected values
expected_values <- chisq.test(values)$expected
#compute chi squared
chi_squared <- chisq.test(values)$statistic
chi_squared <- sum((expected_values-values)^2/expected_values)
#compute contigency coefficiant C
C <- (chi_squared/(sum(values$Pass)+sum(values$Fail)+chi_squared))^0.5
#compute C corrected
C_star <- min(length(values$Pass), ncol(values))
C_corr <- C * (C_star/(C_star-1))^0.5


