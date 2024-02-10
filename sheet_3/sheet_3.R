#SHEET 3
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1.1.
elections <- tibble(Party = character(),Results_2013 = double(),Results_2017 = double())
elections <- elections %>% 
  add_row(Party = "CDU",Results_2013 = 26.8,Results_2017 = 34.1) %>% 
  add_row(Party = "SPD",Results_2013 = 20.5,Results_2017 = 25.7) %>% 
  add_row(Party = "Afd",Results_2013 = 12.6,Results_2017 = 4.7) %>% 
  add_row(Party = "FDP",Results_2013 = 10.7,Results_2017 = 4.8) %>% 
  add_row(Party = "DIELINKE",Results_2013 = 9.2,Results_2017 = 8.6) %>% 
  add_row(Party = "GRUENE",Results_2013 = 8.9,Results_2017 = 8.4) %>% 
  add_row(Party = "CSU",Results_2013 = 6.2,Results_2017 = 7.4) %>% 
  add_row(Party = "Others",Results_2013 = 5.0,Results_2017 = 6.2)

pie(elections$Results_2017,labels = elections$Party)

Results_2013 <- c(elections$Results_2013)
Results_2017 <- c(elections$Results_2017)
Party <- c(elections$Party)
matrix <- cbind(Results_2013,Results_2017)
matrix_t <- t(matrix)

barplot(matrix_t,main = "ElectionResults2013and2017",
        xlab = "Partei",
        ylab = "Ergebnisse",
        col = c("lightblue","lightgreen"),
        legend = row.names(matrix_t),
        beside = TRUE,
        names.arg = Party)

#Aufgabe 1.2.
values <- c(568,577,581,640,641,645,657,673,696,703,720,728,729,777,808,824,825,865,875,1007)
table_values <- table(values)
tibble_values <- tibble(Werte = as.integer(names(table_values)),
                        absolute_Häufigkeit = as.integer(table_values),
                        relative_Häufigkeit = absolute_Häufigkeit/length(table_values),
                        kumulative_Häufigkeit = cumsum(relative_Häufigkeit))

ecdf_data <- ecdf(tibble_values$Werte)
plot(ecdf_data,main = "EmpirischekumulativeVerteilungsfunktion(ECDF)",
     xlab = "Werte",
     ylab = "kumulativeHäufigkeit")

less_equal_800 <- tibble_values %>% filter(Werte <= 800)
ecdf_less_equal_800 <- ecdf(less_equal_800$Werte)
plot(ecdf_less_equal_800)

grater_than_725 <- tibble_values %>% filter(Werte >= 725)
greater_and_less <- tibble_values %>% filter(Werte >= 642 & Werte <= 777)
equal_696 <- tibble_values %>% filter(Werte == 696)
#Ist das so gemeint gewesen?

werte <- c(568,577,581,640,641,645,645,657,673,673,696,703,703,703,703,720,728,729,777,808,824,825,865,875,1007)
table_werte <- table(werte)
tibble_distr_func <- tibble(values = as.integer(names(table_werte)),
                            abs_freq = as.integer(table_werte),
                            rel_freq = abs_freq/sum(table_werte),
                            cum_freq = round(cumsum(rel_freq),2))

ecdf_daten <- ecdf(tibble_distr_func$values)
plot(ecdf_daten)

#Histogramm
class_boundaries <- c(500,600,700,800,900,1000,1100)
hist_data <- hist(werte,breaks = class_boundaries,
                xlab = "Werte",
                ylab = "Frequency",
                col = "Orange")
#Beschriftung der x-Achse anpassen
other_class_bound <- c(500,550,600,700,800,900,1000,1100)
axis(1,at = other_class_bound)

new_class_boundaries <- c(500,600,900,1000,1200)
hist_data <- hist(werte,breaks = new_class_boundaries,
                xlab = "Werte",
                ylab = "Frequency",
                col = "blue")

#Aufgabe 2.1.
#(a)
data_1 <- rnorm(5,mean = 10,sd = 2)
data_2 <- rnorm(5,mean = 10,sd = 8)

#messy tibble der Daten
tab_data <- tibble(num_1 = double(),
                 num_2 = double(),
                 num_3 = double(),
                 num_4 = double(),
                 num_5 = double())

tab_data_fill <- tab_data %>% 
  add_row(num_1 = data_1[1],num_2 = data_1[2],num_3 = data_1[3],num_4 = data_1[4],num_5 = data_1[5]) %>% 
  add_row(num_1 = data_2[1],num_2 = data_2[2],num_3 = data_2[3],num_4 = data_2[4],num_5 = data_2[5])

#tidy tibble der Daten
tidy_data <- tibble(data_1 = rnorm(5,mean = 10,sd = 2),
                    data_2 = rnorm(5,mean = 10,sd = 2))
mean_data_1 <- mean(tidy_data$data_1)
sd_data_1 <- sd(tidy_data$data_1)

#Fuer b unc c hat er einfach manuell erstellt
#(b)
#(c)


#Aufgabe 2.2.
df <- tibble(year = as.double(c(0,1,2,3,4,5)),
           annual_return = c(1,1.13,1.22,1.12,0.95,0.87),
           value = round(cumprod(c(1000,1.13,1.22,1.12,0.95,0.87)),2))
#(b)
vec <- c(1.13,1.22,1.12,0.95,0.87)
geo_mean <- round(prod(vec)^(1/length(vec)),2)

#Aufgabe2.4.
obs <- tibble(Number = c(1:8),abs_freq = c(5,4,1,7,2,3,1,2))
vec_for_geo_mean <- obs$Number^obs$abs_freq
geometric_mean <- prod(vec_for_geo_mean)^(1/sum(obs$abs_freq))
#Test
vec <- c(1,1,1,1,1,2,2,2,2,3,4,4,4,4,4,4,4,5,5,6,6,6,7,8,8)
cum_prod <- prod(vec)
geometric_mean <- cum_prod^(1/length(vec))
#arithemtic mean
vec_for_arith_mean <- rep(obs$Number,obs$abs_freq)
arith_mean <- sum(vec_for_arith_mean)/length(vec_for_arith_mean)
#harmonic mean
vec_repeated <- rep(obs$Number,obs$abs_freq)
vec_for_harmonic_mean <- c(1/vec_repeated)
harmonic_mean <- length(vec_for_harmonic_mean)/sum(vec_for_harmonic_mean)

#Aufgabe 2.3.
obs <- c(4,3,2,4,10)
mean_obs <- mean(obs)
trimmed_mean <- mean(obs,trim = 0.20)
median_obs <- median(obs)
quantile_20 <- quantile(obs,probs = 0.2)
#mode kalkulieren
mode <- obs %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  summarise(abs_freq = n()) %>% 
  filter(abs_freq == max(abs_freq)) %>% 
  pull(value)

#Aufgabe 2.5.
n <- 25
random_sample <- sample(1:20,size = n,replace = TRUE)
#(a)
ecdf_random_sample <- ecdf(random_sample)
plot(ecdf_random_sample)
#(b)
#per Hand
n <- 25
random_sample <- sample(1:20,size = n,replace = TRUE)
sorted_random_sample <- sort(random_sample)
percentile <- 0.30
index <- length(sorted_random_sample)*percentile
quantile_natural_number <- 0.5*(sorted_random_sample[index]+sorted_random_sample[index+1])
quantile_fractional_number <- sorted_random_sample[index+1]
#Funktion
calc.quantile <- function(vector,percentile){
  sorted_vector <- sort(vector)
  index <- length(vector)*percentile
  if(index%%1 != 0){
    return(vector[index+1])
  }else{
    return(0.5*(vector[index]+vector[index+1]))
  }
}
quantile_aus_eigener_funktion <- calc.quantile(vector = sorted_random_sample,percentile = 0.30)
#Funktion aus Library
quantile_aus_libr <- quantile(sorted_random_sample,0.30, type = 1)
#default ist type7 =>lineare interpoltaion
quantile_aus_libr <- quantile(sorted_random_sample,0.30)
#(c)
sample_size <- 10
sorted_random_sample <- sort(sample(x = seq(from = 1, to = 20, by = 1), 
                                    size = sample_size, 
                                    replace = TRUE))
#values und abs_freq in tibble erzeugen mit coun()
tibble_sample <- tibble(values = sorted_random_sample) %>% 
  count(values) %>%
  mutate(rel_freq = tibble_sample$n/sum(n)) %>%
  mutate(cum_freq = cumsum(rel_freq))
#values und abs_freq in tibble erzeugen mit table()
table_sample <- table(sorted_random_sample)
tibble_sample <- tibble(obs = as.integer(names(table_sample)),
                        abs_freq = as.integer(table_sample),
                        rel_freq = abs_freq/sum(abs_freq),
                        cum_freq = cumsum(rel_freq))
#(d)
#hierbei sind die x und y Achse genau anders rum als in der Musterloesung
ecdf_data <- ecdf(tibble_sample$obs)
plot(ecdf_data)
#ich plotte normal, denn fuer die lineare interpolation brauche ich die Obs auf der Y-Achse.
#wir stellen die Werte des Samples in der Y-Achse dar. Als jeweils zugehoerigen X-Wert nehmen 
#wir folgenden Vector  [0, 0.11, 0.22, 0.33, 0.44, 0.56, 0.67, 0.78, 0.89, 1.00]
plot(y = sorted_random_sample, 
     x = 0:(sample_size-1)/(sample_size-1),
     type = "b")
#Wir fuegen nachtraeglich noch Punkte hinzu, welche das Type 7 Quantile des Samples in 5% 
#Schritten darstellt. Wichtig ist hierbei das der vector der x-Achse gleich gross ist wie
#der Vector der Y-Achse denn wir brauchen ja immer einen x und einen y Wert
#type b im Diagram zeigt uns die lineare Interpolation und dadurch ist gut zu sehen dass
#das type=7-Quantile auf der linearen Interpolation basiert.
points(x = seq(0,1, by = 0.05), 
       y = quantile(sorted_random_sample, prob = seq(0,1, by = 0.05)), 
       col = "red")
#Warum genau muessen wir die Formel aus dem Sheet hier als x Werte verwenden?
#Da wir ja die gleiche Anzahl an X und Y Werten nehmen muessen
x <- 0:(sample_size-1)/(sample_size-1)
#(e)
tibble_quantiles <- tibble(points = seq(0,1, by = 0.01),
                           type_7_quantile = quantile(sorted_random_sample, prob = seq(0,1, by = 0.01)),
                           type_1_quantile = quantile(sorted_random_sample, prob = seq(0,1, by = 0.01), type = 1))
#hier wuerde ich gerne nochmal eine Probe machen in dem ich fuer das type_7_quantile in die formel fuer die 
#lineare interpolation einsetze
sample_size <-10
sorted_random_sample <- sort(sample(x = seq(1,20,1), 
                                    size = sample_size, 
                                    replace = TRUE))
plot(x = sorted_random_sample,
     y = 0:(sample_size-1)/(sample_size-1),
     type = "b",
     sub = "black = type 7, blue = type 1",
     main = "comparison type 1 and type 7 quantile",
     xlim=c(0,22), 
     ylim=c(-0.1,1.1))
other_x_axis_boundaries <- c(0:22)
axis(1,at = other_x_axis_boundaries)
#Punkte des type 1 Quantiles
points(y = seq(0,1,0.05), x = quantile(sorted_random_sample, probs = seq(0,1,0.05), type = 1), col = "blue")
#Punkte des type 7 Quantiles
points(y = seq(0,1,0.05), x = quantile(sorted_random_sample, probs = seq(0,1,0.05)), col = "red")
#Was ist eigentlich wenn ich das hier mache?
ecdf_data <- ecdf(tibble_sample$obs)
plot(ecdf_data)
points(y = seq(0,1,0.05), x = quantile(sorted_random_sample, probs = seq(0,1,0.05), type = 1), col = "blue")
points(y = seq(0,1,0.05), x = quantile(sorted_random_sample, probs = seq(0,1,0.05)), col = "red")
#(f) umso groesser das sample ist umso genauer sind die werte aus dem type 1 Quantile an den echten werten
#Aufgabe2.6
tidy_tibble_of_data <- tibble(row_id = rep(1:10,3),
                              player_type = c(rep("non-player", 10), rep("beginner", 10), rep("tournament", 10)),
                              value = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
                                        32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7,
                                        40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
messy_tibble_of_data <- spread(tidy_tibble_of_data, key = player_type, value = value)
#der gleiche dataframe aber anders erstellt
data <- tibble(non_player = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2),
               beginer = c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7),
               tournament = c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3))
#Compare the performance for each group by computing mean, median,
#min, max, quartiles, interquartile range, variance. Create side-by-side
#box plots for these three groups. What can you say about the differences between these groups from the box plots?
measures <- tidy_tibble_of_data %>% 
  group_by(player_type) %>%
  summarise(min = min(value),
            max = max(value),
            mean = mean(value),
            median = median(value),
            var = var(value),
            q1 = quantile(value, 0.25, type = 1),
            q2 = quantile(value, 0.50, type = 1),
            q3 = quantile(value, 0.75, type = 1),
            interquartile_range = q3 - q1)
#simple Boxplot with boxplot()
boxplot(value ~ player_type, data = tidy_tibble_of_data)
#Boxplot with ggplot
ggplot(data = tidy_tibble_of_data) +
  geom_boxplot(mapping = aes(x = player_type, y = value, group = player_type)) +
  geom_point(mapping = aes(x = player_type, y = value, group = player_type)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "",
       x = "",
       y = "")

#Aufgabe 2.7.
#(a) Calculate the arithmetic mean and median for both distance and altitude.
#(b) Calculate the interquartile range and standard deviation for both variables. 
#Compare the variability of both variables.
hikes <-  tibble(year = seq(1,10,1),
                 distance = c(12.5, 29.9, 14.8, 18.7, 7.6, 16.2, 16.5, 27.4, 12.1, 17.5),
                 altitude = c(342, 1245, 502, 555, 398, 670, 796, 912, 238, 466))

measures <- tibble(mean_dist = mean(hikes$distance),
                   median_dist = median(hikes$altitude),
                   q1_dist = quantile(sort(hikes$distance), 0.25, type = 1),
                   q2_dist = quantile(sort(hikes$distance), 0.50, type = 1),
                   q3_dist = quantile(sort(hikes$distance), 0.75, type = 1),
                   interquartile_dist = q3_dist - q1_dist,
                   sd_dist = sd(hikes$distance),
                   var_dist = var(hikes$distance),
                   mean_alt = mean(hikes$altitude),
                   median_alt = median(hikes$altitude),
                   q1_alt = quantile(sort(hikes$altitude), 0.25, type = 1),
                   q2_alt = quantile(sort(hikes$altitude), 0.50, type = 1),
                   q3_alt = quantile(sort(hikes$altitude), 0.75, type = 1),
                   interquartile_alt = q3_alt - q1_alt,
                   sd_alt = sd(hikes$altitude),
                   var_alt = var(hikes$altitude))

#Aufgabe 2.8.
#(a)
?mpg()
#(b) Select only the variables displ (engine displacement) and hwy (highway miles per gallon) from the data set. 
#Group the values of the variable displ into the the groups
#“low” (1 < displ ≤ 3), “medium” (3 < displ ≤ 5) and “big” (5 < displ ≤ 8). 
#Use the cut() command to do this. 
#Add a column displ class which denotes the belonging to one of the groups
mpg_data <- mpg
df <- mpg_data %>%
  tibble() %>%
  select(displ, hwy) %>%
  mutate(displ_class = cut(x = displ,
                           breaks = c(1, 3, 5, 8),
                           labels = c("niedrig", "mittel", "gross"),
                           include.lowest = TRUE))
#(c) Calculate the mean, minimum, maximum and the three quartile of
#the variable hwy depending on the values of displ and depending on displ class.
measures_df <- df %>%
  group_by(displ) %>%
  summarise(mean = mean(hwy),
            min = min(hwy),
            max = max(hwy),
            q1 = quantile(hwy, 0.25, type = 1),
            q2 = quantile(hwy, 0.50, type = 1),
            q3 = quantile(hwy, 0.75, type = 1),
            n_obs = n())
#Aus der Aufgabenstellung (b) und (c) war sehr schwer rauszuerkennen wie die tibble am Ende aussehen soll

#(c)Boxplots
boxplot(df$hwy ~ df$displ, data = df)
boxplot(df$hwy ~ df$displ_class, data = df)
#Man kann deutlich erkennen dass die Boxplots aussagekraeftiger sind wenn nach der dsiplay class gruppiert wird
#anstatt nach den Hubraum zu gruppieren. 
#Boxplots mit ggplot
ggplot(data = df) +
  geom_boxplot(mapping = aes(x = df$displ, y = df$hwy, group = displ)) +
  geom_point(mapping = aes(x = df$displ, y = df$hwy, group = displ))

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = df$displ_class, y = df$hwy, group = displ_class)) +
  geom_point(mapping = aes(x = df$displ_class, y = df$hwy, group = displ_class))
  
#Aufgabe 3.1.
#Antwort: a) 3, b) 2, c) 2, d) 2, e) 3
#         f) verstehe nicht warum die alle symmetric sind, woran erkenne ich das?

#Aufgabe 3.2. 
#left skewed










