library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Descriptive Statistics
#read the csv file into a tibble
path_dataset <- "C:/Users/sebas/OneDrive/Dokumente/GitHub/Statistics_WS23/exam_data.csv"
data_set <- read.csv(path_dataset) %>% as_tibble()
#split the Variable exam into mat.nr and course
tidy_data <- data_set %>%
  separate(exam, into = c("mat.nr", "course"), sep = "/")
#add new variable grade
new_variable <- tidy_data %>%
  mutate(grade = case_when(score < 50 ~ 5, 
                           score >= 50 & score < 65 ~ 4,
                           score >= 65 & score < 80 ~ 3,
                           score >= 80 & score < 90 ~ 2,
                           score >= 90 ~ 1))
  
#determine the total number of tests in each exam and number of students participating
total_number_of_tests <- new_variable %>% 
  group_by(course) %>%
  summarise(n = n())

unique_students <- new_variable %>%
  group_by(mat.nr) %>% 
  summarise(n = n())

count(unique_students)
total_number_of_students <- count(unique_students)

#For each subject, determine the absolute frequencies of the grades
#and store the result in a tibble with the variables grade, Computer
#Networks, Data Bases, Formal Languages, Mathematics, OOP and
#Software Engineering.
notenverteilungen <- new_variable %>%
  count(course, grade) %>%
  spread(key = course, value = n)

#For each subject, determine the minimum, maximum, the three
#quartiles, the mean of the variable score, the number of 
#participants and the dropout rates.
measures <- new_variable %>%
  group_by(course) %>%
   summarise(minimum = min(score),
             maximum = max(score),
             mean = mean(score),
             participants = n(),
             q1 = quantile(score, 0.25, type = 1),
             q2 = quantile(score, 0.50, type = 1),
             q3 = quantile(score, 0.75, type = 1))

dropouts <- new_variable %>%
  filter(attempt == 3, grade == 5)

number_of_dropouts_as_tibble <- count(dropouts)
#oder
number_of_dropouts_as_int <- as.integer(count(dropouts))

#bessere Lösung
measures <- new_variable %>%
  mutate(dropout = if_else(grade <= 4, 0, 1)) %>%
  group_by(course) %>%
  summarise(minimum = min(score),
            maximum = max(score),
            mean = mean(score),
            participants = n(),
            q1 = quantile(score, 0.25, type = 1),
            q2 = quantile(score, 0.50, type = 1),
            q3 = quantile(score, 0.75, type = 1),
            dropout_rate = sum(dropout)/participants)
# Create side by side boxplots of the score for each subject and
#interpret the results
boxplot(new_variable$score ~ new_variable$course, 
        data = new_variable,
        main = "Side by Side Boxplots",
        xlab = "",
        ylab = "score")

# Determine the contingency table of the variables attempt 
#and grade and determine the indifference table and chi-square value.
contigency_table_observed <- chisq.test(new_variable$attempt, new_variable$grade)$observed %>%
  addmargins()

contigency_table_expected <- chisq.test(new_variable$attempt, new_variable$grade)$expected %>%
  addmargins()

X_squared <- chisq.test(new_variable$attempt, new_variable$grade)$statistic

#Inferentielle Statistics
#Aufgabe 4
befragung <- c(1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0,
               1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0,
               0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0,
               0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0,
               0, 0, 1, 0, 1, 0, 0, 0)
#(a)
rel_freq_A <- sum(befragung) / length(befragung)
expected_value <- length(befragung) * rel_freq_A

#(c)
p.hat <- rel_freq_A
n <- 100
q <- qnorm(1-alpha/2)
#Approximation
u.apr <- p.hat - q * sqrt(p.hat * (1-p.hat) / 100)
o.apr <- p.hat - q * sqrt(p.hat * (1-p.hat) / 100)
#Exact Values
binom.test(x=32, n=100, alternative = "two.sided", conf.level = 1-alpha)$conf.int
t.test(befragung, alternative = "two.sided", conf.level = 1-alpha)



qnorm(0.95, p = 0.5)




