library("tidyverse")
library("tidyr")
library("dplyr")
library("nycflights13")


#Aufgabe
#Was sind Observations, was sind Variables und wann ist ein Datensatz tidy
#Jede Variable bildet eine Spalte: In einem tidy dataset sollten die Variablen oder Merkmale, 
#die Sie analysieren möchten, in separaten Spalten dargestellt werden. Das bedeutet, 
#dass jede Spalte eine bestimmte Art von Information enthält.
#Jede Beobachtung bildet eine Zeile: Jede Zeile des Datasets sollte eine einzelne Beobachtung
#oder einen einzelnen Datensatz repräsentieren. Dies bedeutet, dass jede Zeile alle relevanten 
#Informationen für diese spezifische Beobachtung enthält.
#Jeder Wert befindet sich an einer bestimmten Stelle im Dataset: Jeder Wert in Ihrem Dataset 
#sollte in der Zeile und Spalte sein, die dieser Wert repräsentiert. Dies sorgt für Klarheit und Verständlichkeit.
#Die Daten sind homogen: In einem tidy dataset sollten die Daten in jeder Spalte den gleichen Datentyp aufweisen. 
#Dies bedeutet, dass numerische Werte in numerischen Spalten und kategoriale Werte in kategorialen Spalten sein sollten.

#Datensatz "student1"
student1 <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
#Datensatz "student1" ist nicht tidy da ein und dieselbe Variable in mehreren Spalten vorkommt 
#So wuerde der Datensatz in tidy aussehen:
student1 <- tibble(student = character(), subject = character(), score = double())

student1 <- student1 %>%
  add_row(student = "Adam", subject = "Algebra", score = NA) %>%
  add_row(student = "Adam", subject = "Analysis", score = 2) %>%
  add_row(student = "Adam", subject = "Diskrete Mathematik", score = 3) %>%
  add_row(student = "Bernd", subject = "Algebra", score = 5) %>%
  add_row(student = "Bernd", subject = "Analysis", score = NA) %>%
  add_row(student = "Bernd", subject = "Diskrete Mathematik", score = NA) %>%
  add_row(student = "Christian", subject = "Algebra", score = 3) %>%
  add_row(student = "Christian", subject = "Analysis", score = 1) %>%
  add_row(student = "Christian", subject = "Diskrete Mathematik", score = 2) %>%
  add_row(student = "Doris", subject = "Algebra", score = 4) %>%
  add_row(student = "Doris", subject = "Analysis", score = 3) %>%
  add_row(student = "Doris", subject = "Diskrete Mathematik", score = 4)
print(student1)

#Datensatz "student2"
student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62)
)
#Datensatz "student2" ist nicht tidy, da in der Spalte "type" mehrere Variablen auffinden
#So wuerde der Datensatz in tidy aussehen:
student2 <- tibble(name = character(), height = double(), weight = double())
student2 <- student2 %>%
  add_row(name = "Adam", height = 1.83, weight = 81) %>%
  add_row(name = "Bernd", height = 1.75, weight = 71) %>%
  add_row(name = "Christian", height = 1.65, weight = 55) %>%
  add_row(name = "Doris", height = 1.57, weight = 62)
print(student2)

#Datensatz "student3"
student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57")
)

#Datensatz "student3" ist nicht tidy da ein und dieselbe Variable in mehreren Spalten vorkommt 
#So wuerde der Datensatz in tidy aussehen:
student3 <- tibble(name = character(), height = double(), weight = double())
student3 <- student3 %>%
  add_row(name = "Adam", height = 1.83, weight = 81) %>%
  add_row(name = "Bernd", height = 1.75, weight = 71) %>%
  add_row(name = "Christian", height = 1.65, weight = 55) %>%
  add_row(name = "Doris", height = 1.57, weight = 62)
print(student3)




#with gather() and spread()
student1 <- student1 %>% gather(key = "subject", value = "score", 2:4)
student2 <- student2 %>% spread(key = type, value = measure)

#weiters Beispiel fuer spread()
data_long <- tibble(
  Person = c("Alice", "Alice", "Bob", "Bob"),
  Subject = c("Math", "Science", "Math", "Science"),
  Score = c(90, 88, 85, 76)
)
data_wide <- data_long %>% spread(key = Subject, value = Score)


#Aufgabe 2
result <- sin(log(sqrt(5 + 3)))
vector <- seq(from=0.5, to=5, by=0.5)
result_nested <- sum(round(log(vector^2), 2))
result_pipe <- vector %>%
  '^'(2) %>%
  log() %>%
  round(2) %>%
  sum()

#Aufgabe 3 
df <- tibble(id = 1:10, 
             sex = sample(x = c("f", "m"), size = 10, replace = TRUE),
             age = round(runif(n = 10, min = 20, max = 35)), 
             score1 = round(runif(n = 10, min = 0, max = 25))
)

#Select the date of all male students
selected_data <- df %>% filter(sex == "m")
#Add the data of a new student with id = 11, sex = “m”, age = 25 and score1 = 4.
df <- df %>%
  add_row(id = 11, sex = "m", age = 25, score1 = 4)

#Add two columns score2 and score3 with random integer numbers between 0 and 25.
df <- df %>% mutate(score2 = sample(0:25, size = 11, replace = TRUE),
                    score3 = sample(0:25, size = 11, replace = TRUE))
#Add a column containing sum of all scores
df <- df %>% rowwise() %>% mutate(score_sum = sum(score1, score2, score3))
df <- df %>% mutate(score_sum = rowSums(select(df, score1:score3)))
df <- df %>% mutate(score_sum = rowSums(select(df, -id, -sex, -age)))
df <- df %>% mutate(score_sum = rowSums(select(df, score1, score2, score3)))
#Add a column which denote the grades 
df <- df %>% mutate(grade = if(score_sum <= 37) 5
                    else if(score_sum > 37 & score_sum <= 45) 4
                    else if(score_sum > 45 & score_sum <= 55) 3
                    else if(score_sum > 55 & score_sum <=65) 2
                    else (score_sum > 65))

#Find the values of the variables id, sex and grade sorted by the values of sex of all students who have passed.
#Uebersicht der Studenten welche bestanden haben nach Geschlecht
students_passed <- df %>% select(id, sex, grade) %>% filter(grade <= 4) %>% arrange(sex)

#Calculate the mean, minimum, maximum and median of the variable sum of scores grouped by the variable sex.
clacultaions <- df %>% group_by(sex) %>% summarise(Durchschnitt = mean(score_sum),
                                         Minimum = min(score_sum),
                                         Maximum = max(score_sum),
                                         median(score_sum))

#Aufgabe 4
no <- 30
exercise.results <- tibble(
  stud.id = 1:no,
  group = sample(x=c("A","B","C"), size=no, replace = TRUE),
  ex1 = sample(x=1:10, size=no, replace = TRUE),
  ex2= sample(x=1:10, size=no, replace = TRUE),
  ex3 = sample(x=1:10, size=no, replace = TRUE),
  ex4 = sample(x=1:10, size=no, replace = TRUE),
  ex5 = sample(x=1:10, size=no, replace = TRUE)
)
#Apply n() and count() to get the number of students in the different groups. What are the difference between n() and count()?
#count() <=> group_by() <=> summatrise(Anzahl = n())
group_distribution <- exercise.results %>% group_by(group) %>% summarise(Anzahl = n())
group_distribution <- exercise.results %>% count(group)

#Aufgabe 5
flights <- flights
#Find all flights with more than 2 hours arrival delay
x <- flights %>% filter(arr_delay > 2)
#Find all flights with more tahn 2 hours arrival delay and no departure delay
y <- flights %>% filter(arr_delay > 2 &  dep_delay <= 0)
#Find all flights from United, American and Delta with no arrival delay
z <- flights %>% filter((carrier == "UA" | carrier == "AA") & arr_delay <= 0)
#Find all flights from United, American and Delta in the month
#May with more than 5 hours arrival delay sorted by carrier and flight number.
w <- flights %>% filter((carrier == "UA" | carrier == "AA" | carrier == "DL") & arr_delay >= 5) %>% arrange(carrier, flight)
#Add a column speed which denotes the average speed of the flight and determine the carrier, flight of the top 10 values of speed.
flights <- flights %>% mutate(air_time_hours = round((air_time / 60), 2))
flights <- flights %>% mutate(speed = round((distance/air_time_hours), 2))
top10 <- flights %>% select(carrier, flight, speed) %>% arrange(desc(speed)) %>% head(10)
#Find a list of carriers with a column ratio which denotes the number of flights with arr delay less than 10 minutes 
#to the total number of flights. The list should be sorted by ratio
h <- flights %>% group_by(carrier) %>% summarise(ans_flights = n(),
                                                 ans_flights_no_delay = sum(arr_delay<10, na.rm = TRUE),
                                                 ratio_delayed_flights = round(ans_flights_no_delay / ans_flights, 2)) %>% arrange(desc(ratio_delayed_flights))
                                                 
#Determine a table that shows, for each airline (carrier), the flight
#connection given by the airports of dest und origin that occurred
#most frequently in 2013. The table should contain only the columns
#names of airline, destination, origin and frequency and be sorted
#by frequency in descending order. You can find the names of the
#carrier from the dataset airlines and the names of the airports
#from the dataset airports.
airports <- airports
airlines <- airlines
flights_2013 <- flights %>% filter(year == 2013)
flights_frequency <- flights_2013 %>% group_by(carrier, origin, dest) %>% summarise(frequency = n()) %>% arrange(desc(frequency))
flights_frequency_with_names <- left_join(flights_frequency, airlines, by = c("carrier" = "carrier"))
colnames(flights_frequency_with_names) <- c("abbr", "origin", "dest", "freq", "carrier_name")
flights_frequency_with_names_ordered <- flight_counts_with_names %>% select(abbr, carrier_name, origin, dest, freq)









