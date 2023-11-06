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
vector <- c(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808, 824, 825, 865, 875, 1007)
tab <- table(vector)
cum_sum <- cumsum(tab)
plot(names(cum_sum), cum_sum, type = "s", 
     main="Empirische kumulative Verteilungsfunktion (ECDF)", 
     xlab="Werte")


#Beispiel-Vektor erstellen
data <- c(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808, 824, 825, 865, 875, 1007)
#Erstelle die ECDF
ecdf_data <- ecdf(data)
#Plotte die ECDF
plot(ecdf_data, main="Empirische kumulative Verteilungsfunktion (ECDF)", xlab="Werte")

#Aufgabe 4
obs <- tibble(Number = c(1:8), abs_freq = c(5, 4, 1, 7, 2, 3, 1, 2))
arith_mean <- mean(obs$abs_freq)
geometric_mean <- exp(mean(log(obs$abs_freq)))



