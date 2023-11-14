library("tidyverse")
library("tidyr")
library("dplyr")
library("nycflights13")

#Sheet 3
#Aufgabe 1
#Summarize the results of 2017 in a pie and abar chart. Compare the
#results in 2013 and 2017 with an appropriate bar chart
elections <- tibble(Party = character(), Results_2013 = double(), Results_2017 = double())
elections <- elections %>% 
  add_row(Party = "CDU", Results_2013 = 26.8, Results_2017 = 34.1) %>%
  add_row(Party = "SPD", Results_2013 = 20.5, Results_2017 = 25.7) %>%
  add_row(Party = "Afd", Results_2013 = 12.6, Results_2017 = 4.7) %>%
  add_row(Party = "FDP", Results_2013 = 10.7, Results_2017 = 4.8) %>%
  add_row(Party = "DIE LINKE", Results_2013 = 9.2, Results_2017 = 8.6) %>%
  add_row(Party = "GRUENE", Results_2013 = 8.9, Results_2017 = 8.4) %>%
  add_row(Party = "CSU", Results_2013 = 6.2, Results_2017 = 7.4) %>%
  add_row(Party = "Others", Results_2013 = 5.0, Results_2017 = 6.2)
#Summarize the results of 2017 in a pie and abar chart. Compare the
#results in 2013 and 2017 with an appropriate bar chart.
pie(elections$Results_2017, labels = elections$Party)

Results_2013 <- c(elections$Results_2013)
Results_2017 <- c(elections$Results_2017)
Party <- c(elections$Party)
matrix <- cbind(Results_2013, Results_2017)
matrix_t <- t(matrix)

barplot(matrix_t, main = "Election Results 2013 and 2017",
                xlab = "Partei",
                ylab = "Ergebnisse",
                col = c("lightblue", "lightgreen"),
                legend = row.names(matrix_t),
                beside = TRUE,
                names.arg = Party)

#Aufgabe 2
values <- c(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808, 824, 825, 865, 875, 1007)
table_values <- table(values)
tibble_values <- tibble(Werte = as.integer(names(table_values)),
                        absolute_Häufigkeit = as.integer(table_values),
                        relative_Häufigkeit = absolute_Häufigkeit / length(table_values),
                        kumulative_Häufigkeit = cumsum(relative_Häufigkeit))

ecdf_data <- ecdf(tibble_values$Werte)
plot(ecdf_data, main = "Empirische kumulative Verteilungsfunktion (ECDF)",
                xlab = "Werte",
                ylab = "kumulative Häufigkeit")

#Compute using the cumulative frequency distribution the proportion of response times
less_equal_800 <- tibble_values %>% filter(Werte <= 800) 
ecdf_less_equal_800 <- ecdf(less_equal_800$Werte)
plot(ecdf_less_equal_800)

grater_than_725 <- tibble_values %>% filter(Werte >= 725)
greater_and_less <- tibble_values %>% filter(Werte >= 642 & Werte <= 777)
equal_696 <- tibble_values %>% filter(Werte == 696)
#Ist das so gemeint gewesen?

#Hier habe ich wiederholt was bei Aufgabe 2a gefragt ist da hier nach "draw the distribution function" gefragt ist.
#So wie ich verstanden habe moechte er hier ein histogramm und eine Verteilungsfunktion mit den richtigen class boundaries
werte <- c(568, 577, 581, 640, 641, 645, 645, 657, 673, 673, 696, 703, 703, 703, 703, 720, 728, 729, 777, 808, 824, 825, 865, 875, 1007) 
table_werte <- table(werte)
tibble_distr_func <- tibble(values = as.integer(names(table_werte)),
                            abs_freq = as.integer(table_werte),
                            rel_freq = abs_freq / sum(table_werte),
                            cum_freq = round(cumsum(rel_freq), 2))

ecdf_daten <- ecdf(tibble_distr_func$values)
plot(ecdf_daten)



#Histogramm
class_boundaries <- c(500, 600, 700, 800, 900, 1000, 1100)
hist_data <- hist(werte, breaks = class_boundaries,
                          xlab = "Werte",
                          ylab = "Frequency",
                          col = "Orange")
#so kann ich die Beschriftung der x-Achse verändern
other_class_bound <- c(500, 550, 600, 700, 800, 900, 1000, 1100)
axis(1, at = other_class_bound)

#Warum bekomme ich hier so komische Werte in der Y-Achse?
new_class_boundaries <- c(500, 600, 900, 1000, 1200)
hist_data <- hist(werte, breaks = new_class_boundaries,
                          xlab = "Werte",
                          ylab = "Frequency",
                          col = "blue")

#Aufgabe 2.1
#Make up data sets with 5 numbers each that have:
#(a) the same mean but different standard deviations.
data_1 <- rnorm(5, mean = 10, sd = 2)
data_2 <- rnorm(5, mean = 10, sd = 8)

#messy tibble der Daten
tab_data <- tibble(num_1 = double(),
                   num_2 = double(),
                   num_3 = double(),
                   num_4 = double(),
                   num_5 = double())
tab_data_fill <- tab_data %>% 
  add_row(num_1 = data_1[1], num_2 = data_1[2], num_3 = data_1[3], num_4 = data_1[4], num_5 = data_1[5]) %>%
  add_row(num_1 = data_2[1], num_2 = data_2[2], num_3 = data_2[3], num_4 = data_2[4], num_5 = data_2[5])

#tidy tibble der Daten
tidy_data <- tibble(data_1 = rnorm(5, mean = 10, sd = 2),
                    data_2 = rnorm(5, mean = 10, sd = 2))
mean_data_1 <- mean(tidy_data$data_1)
sd_data_1 <- sd(tidy_data$data_1)

#(b) the same mean but different medians.

#(c) the same median but different means.


#Aufgabe 2.2
#Consider a stock portfolio that began with a value of 1000 $ and had
#annual returns of 13%, 22%, 12%, -5%, and -13%.
#(a) Compute the value after each of the five years.
df <- tibble(year = as.double(c(0, 1, 2, 3, 4, 5)),
             annual_return = c(1, 1.13, 1.22, 1.12, 0.95, 0.87),
             value = round(cumprod(c(1000, 1.13, 1.22, 1.12, 0.95, 0.87)), 2))
#(b) Compute the annual rate of return.
#Use the geometric mean
vec <- c(1.13, 1.22, 1.12, 0.95, 0.87)
geo_mean <- round(prod(vec)^(1/length(vec)), 2)
#(c) Based on the result of (b), which annual returns do you expect in the next two years? 
#Would it make sense to predict the annual return 20 years later?
#Based on the result the next annual return will be 5%
#Would it make sense to prdeict the annual return 20 years later? - verstehe diese Frage nicht


#Aufgabe 4
obs <- tibble(Number = c(1:8), abs_freq = c(5, 4, 1, 7, 2, 3, 1, 2))
vec_for_geo_mean <- obs$Number^obs$abs_freq
geometric_mean <- prod(vec_for_geo_mean)^(1/sum(obs$abs_freq))
#Test
vec <- c(1,1,1,1,1,2,2,2,2,3,4,4,4,4,4,4,4,5,5,6,6,6,7,8,8)
cum_prod <- prod(vec)
geometric_mean <- cum_prod^(1/length(vec))
#arithemtic mean
vec_for_arith_mean <- rep(obs$Number, obs$abs_freq) 
arith_mean <- sum(vec_for_arith_mean)/length(vec_for_arith_mean)
#harmonic mean
vec_repeated <- rep(obs$Number, obs$abs_freq)
vec_for_harmonic_mean <- c(1/vec_repeated)
harmonic_mean <- length(vec_for_harmonic_mean)/sum(vec_for_harmonic_mean)





